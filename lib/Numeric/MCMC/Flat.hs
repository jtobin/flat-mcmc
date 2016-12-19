{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
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

  , VE.ensemble
  , VE.particle
  ) where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Par (NFData)
import Control.Monad.Par.Combinator (parMap)
import Control.Monad.Par.Scheds.Sparks hiding (get)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.Trans.State.Strict (get, put, execStateT)
import Data.Monoid
import Data.Sampling.Types as Sampling.Types hiding (Chain(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Extended as VE (ensemble, particle)
import qualified Data.Vector.Unboxed as U
import Formatting ((%))
import qualified Formatting as F
import Pipes (Producer, lift, yield, runEffect, (>->))
import qualified Pipes.Prelude as Pipes
import System.Random.MWC.Probability as MWC

data Chain = Chain {
    chainTarget   :: Target Particle
  , chainPosition :: !Ensemble
  }

-- | Render a Chain as a text value.
render :: Chain -> T.Text
render Chain {..} = renderEnsemble chainPosition
{-# INLINE render #-}

renderParticle :: Particle -> T.Text
renderParticle =
      T.drop 1
    . U.foldl' glue mempty
  where
    glue = F.sformat (F.stext % "," % F.float)
{-# INLINE renderParticle #-}

renderEnsemble :: Ensemble -> T.Text
renderEnsemble =
      T.drop 1
    . V.foldl' glue mempty
  where
    glue a b = a <> "\n" <> renderParticle b
{-# INLINE renderEnsemble #-}

-- | A particle is an n-dimensional point in Euclidean space.
--
--   You can create a particle by using the 'particle' helper function, or just
--   use Data.Vector.Unboxed.fromList.
type Particle = U.Vector Double

-- | An ensemble is a collection of particles.
--
--   The Markov chain we're interested in will run over the space of ensembles,
--   so you'll want to build an ensemble out of a reasonable number of
--   particles to kick off the chain.
--
--   You can create an ensemble by using the 'ensemble' helper function, or just
--   use Data.Vector.fromList.
type Ensemble = Vector Particle

symmetric :: PrimMonad m => Prob m Double
symmetric = fmap transform uniform where
  transform z = 0.5 * (z + 1) ^ (2 :: Int)
{-# INLINE symmetric #-}

stretch :: Particle -> Particle -> Double -> Particle
stretch p0 p1 z = U.zipWith str p0 p1 where
  str x y = z * x + (1 - z) * y
{-# INLINE stretch #-}

acceptProb :: Target Particle -> Particle -> Particle -> Double -> Double
acceptProb target particle proposal z =
    lTarget target proposal
  - lTarget target particle
  + log z * (fromIntegral (U.length particle) - 1)
{-# INLINE acceptProb #-}

move :: Target Particle -> Particle -> Particle -> Double -> Double -> Particle
move target !p0 p1 z zc =
  let !proposal = stretch p0 p1 z
      pAccept  = acceptProb target p0 proposal z
  in  if   zc <= min 1 (exp pAccept)
      then proposal
      else p0
{-# INLINE move #-}

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
  js  <- U.replicateM n (uniformR (1, n))

  let granularity = n `div` 2

      w0 k    = e0 `V.unsafeIndex` pred k
      w1 k ks = e1 `V.unsafeIndex` pred (ks `U.unsafeIndex` pred k)

      worker (k, z, zc) = move target (w0 k) (w1 k js) z zc
      !result = runPar $
        parMapChunk granularity worker (zip3 [1..n] zs zcs)

  return $! V.fromList result
{-# INLINE execute #-}

-- | The 'flat' transition operator for driving a Markov chain over a space
--   of ensembles.
flat
  :: PrimMonad m
  => Transition m Chain
flat = do
  Chain {..} <- get
  let size = V.length chainPosition
      n    = truncate (fromIntegral size / 2)
      e0   = V.unsafeSlice 0 n chainPosition
      e1   = V.unsafeSlice n n chainPosition
  result0 <- lift (execute chainTarget e0 e1 n)
  result1 <- lift (execute chainTarget e1 result0 n)
  let !ensemble = V.concat [result0, result1]
  put $! (Chain chainTarget ensemble)
{-# INLINE flat #-}

chain :: PrimMonad m => Chain -> Gen (PrimState m) -> Producer Chain m ()
chain = loop where
  loop state prng = do
    next <- lift (MWC.sample (execStateT flat state) prng)
    yield next
    loop next prng
{-# INLINE chain #-}

-- | Trace 'n' iterations of a Markov chain and stream them to stdout.
--
--   Note that the Markov chain is defined over the space of ensembles, so
--   you'll need to provide an ensemble of particles for the start location.
--
-- >>> import Numeric.MCMC.Flat
-- >>> import Data.Vector.Unboxed (toList)
-- >>> :{
-- >>>   let rosenbrock xs = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)
--             where [x0, x1] = toList xs
-- >>> :}
-- >>> :{
-- >>> let origin = ensemble [
-- >>>       particle [negate 1.0, negate 1.0]
-- >>>     , particle [negate 1.0, 1.0]
-- >>>     , particle [1.0, negate 1.0]
-- >>>     , particle [1.0, 1.0]
-- >>>     ]
-- >>> :}
-- >>> withSystemRandom . asGenIO $ mcmc 2 origin rosenbrock
-- -1.0,-1.0
-- -1.0,1.0
-- 1.0,-1.0
-- 0.7049046915549257,0.7049046915549257
-- -0.843493377618159,-0.843493377618159
-- -1.1655594505975082,1.1655594505975082
-- 0.5466534497342876,-0.9615123448709006
-- 0.7049046915549257,0.7049046915549257
mcmc
  :: (MonadIO m, PrimMonad m)
  => Int
  -> Ensemble
  -> (Particle -> Double)
  -> Gen (PrimState m)
  -> m ()
mcmc n chainPosition target gen = runEffect $
        chain Chain {..} gen
    >-> Pipes.take n
    >-> Pipes.mapM_ (liftIO . T.putStrLn . render)
  where
    chainTarget = Target target Nothing
{-# INLINE mcmc #-}

-- A parallel map with the specified granularity.
parMapChunk :: NFData b => Int -> (a -> b) -> [a] -> Par [b]
parMapChunk n f xs = concat <$> parMap (map f) (chunk n xs) where
  chunk _ [] = []
  chunk m ys =
    let (as, bs) = splitAt m ys
    in  as : chunk m bs
{-# INLINE parMapChunk #-}

