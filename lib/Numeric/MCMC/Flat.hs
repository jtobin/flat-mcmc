{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module: Numeric.MCMC.Flat
-- Copyright: (c) 2016 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>
-- Stability: unstable
-- Portability: ghc
--
-- This is the 'affine invariant ensemble' or AIEMCMC algorithm described in
-- Goodman and Weare, 2010.  It is a reasonably efficient and hassle-free
-- sampler, requiring no mucking with tuning parameters or local proposal
-- distributions.
--
-- The 'mcmc' function streams a trace to stdout to be processed elsewhere,
-- while the `flat` transition can be used for more flexible purposes,
-- such as working with samples in memory.
--
-- See <http://msp.org/camcos/2010/5-1/camcos-v5-n1-p04-p.pdf> for the definitive
-- reference of the algorithm.

module Numeric.MCMC.Flat (
    mcmc
  , flat
  , Particle
  , Ensemble
  , Chain

  , module Sampling.Types
  , MWC.create
  , MWC.createSystemRandom
  , MWC.withSystemRandom
  , MWC.asGenIO
  ) where

import Control.Monad (replicateM)
import Control.Monad.Par (NFData)
import Control.Monad.Par.Scheds.Direct hiding (put, get)
import Control.Monad.Par.Combinator (parMap)
import Control.Monad.Primitive (PrimMonad, PrimState, RealWorld)
import Control.Monad.Trans.State.Strict (get, put, execStateT)
import Data.Sampling.Types as Sampling.Types hiding (Chain(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Pipes (Producer, lift, yield, runEffect, (>->))
import qualified Pipes.Prelude as Pipes
import System.Random.MWC.Probability as MWC

data Chain = Chain {
    chainTarget   :: Target Particle
  , chainPosition :: !Ensemble
  }

instance Show Chain where
  show Chain {..} =
      init
    . filter (`notElem` "[]")
    . unlines
    . V.toList
    . V.map show
    $ chainPosition

type Particle = Vector Double

type Ensemble = Vector Particle

symmetric :: PrimMonad m => Prob m Double
symmetric = fmap transform uniform where
  transform z = 0.5 * (z + 1) ^ (2 :: Int)

stretch :: Particle -> Particle -> Double -> Particle
stretch p0 p1 z = V.zipWith (+) (V.map (* z) p0) (V.map (* (1 - z)) p1)

acceptProb :: Target Particle -> Particle -> Particle -> Double -> Double
acceptProb target particle proposal z =
    lTarget target proposal
  - lTarget target particle
  + log z * (fromIntegral (V.length particle) - 1)

move :: Target Particle -> Particle -> Particle -> Double -> Double -> Particle
move target p0 p1 z zc =
  let proposal = stretch p0 p1 z
      pAccept  = acceptProb target p0 proposal z
  in  if   zc <= min 1 (exp pAccept)
      then proposal
      else p0

execute
  :: PrimMonad m
  => Target Particle
  -> Ensemble
  -> Ensemble
  -> Int
  -> Prob m Ensemble
execute target e0 e1 n = do
  zs  <- replicateM n symmetric
  zcs <- replicateM n uniform
  vjs <- replicateM n (uniformR (1, n))

  let granularity = truncate (fromIntegral n / 2)

      js      = U.fromList vjs
      w0 k    = e0 `V.unsafeIndex` pred k
      w1 k ks = e1 `V.unsafeIndex` pred (ks `U.unsafeIndex` pred k)


      worker (k, z, zc) = move target (w0 k) (w1 k js) z zc
      result = runPar $
        parMapChunk granularity worker (zip3 [1..n] zs zcs)

  return $ V.fromList result

-- | The 'flat' transition operator for driving a Markov chain over a space
--   of ensembles.
flat
  :: PrimMonad m
  => Transition m Chain
flat = do
  Chain {..} <- get
  let size = V.length chainPosition
      n    = truncate (fromIntegral size / 2)
      e0   = V.slice 0 n chainPosition
      e1   = V.slice n n chainPosition
  result0 <- lift (execute chainTarget e0 e1 n)
  result1 <- lift (execute chainTarget e1 result0 n)
  let ensemble = V.concat [result0, result1]
  put (Chain chainTarget ensemble)

chain :: PrimMonad m => Chain -> Gen (PrimState m) -> Producer Chain m ()
chain = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT flat state) prng)
    yield next
    loop next prng

-- | Trace 'n' iterations of a Markov chain and stream them to stdout.
--
--   Note that the Markov chain is defined over the space of ensembles, so
--   you'll need to provide an ensemble of particles for the start location.
--
-- >>> import Numeric.MCMC.Flat
-- >>> import Data.Vector (Vector, toList, fromList)
-- >>> :{
-- >>>   let rosenbrock xs = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)
--             where [x0, x1] = toList xs
-- >>> :}
-- >>> :{
-- >>> let ensemble = fromList [
-- >>>       fromList [negate 1.0, negate 1.0]
-- >>>     , fromList [negate 1.0, 1.0]
-- >>>     , fromList [1.0, negate 1.0]
-- >>>     , fromList [1.0, 1.0]
-- >>>     ]
-- >>> :}
-- >>> withSystemRandom . asGenIO $ mcmc 2 ensemble rosenbrock
-- -1.0,-1.0
-- -1.0,1.0
-- 1.0,-1.0
-- 0.7049046915549257,0.7049046915549257
-- -0.843493377618159,-0.843493377618159
-- -1.1655594505975082,1.1655594505975082
-- 0.5466534497342876,-0.9615123448709006
-- 0.7049046915549257,0.7049046915549257
mcmc :: Int -> Ensemble -> (Particle -> Double) -> Gen RealWorld -> IO ()
mcmc n chainPosition target gen = runEffect $
        chain Chain {..} gen
    >-> Pipes.take n
    >-> Pipes.mapM_ print
  where
    chainTarget = Target target Nothing

-- A parallel map with the specified granularity.
parMapChunk :: NFData b => Int -> (a -> b) -> [a] -> Par [b]
parMapChunk n f xs = concat <$> parMap (map f) (chunk n xs) where
  chunk _ [] = []
  chunk m ys =
    let (as, bs) = splitAt m ys
    in  as : chunk m bs

