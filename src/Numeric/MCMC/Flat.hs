{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Numeric.MCMC.Flat (
    Chain(..)
  , Options(..)
  , Ensemble
  , Density
  , flat
  -- * System.Random.MWC
  , sample
  , withSystemRandom
  , asGenIO
  , asGenST
  , create
  , initialize
  ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Par (NFData)
import Control.Monad.Par.Scheds.Direct hiding (put, get)
import Control.Monad.Par.Combinator
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.Random.MWC.Probability

-- | The state of a Markov chain.
--
--   The target function - assumed to be proportional to a log density - is
--   itself stateful, allowing for custom annealing schedules if you know what
--   you're doing.
data Chain = Chain {
    logObjective :: Density
  , ensemble     :: Ensemble      
  , accepts      :: !Int
  , iterations   :: !Int
  }

instance Show Chain where
  show c = show . unlines . map (sanitize . show) $ us  where
    us = V.toList e
    e  = ensemble c
    sanitize = filter (`notElem` "fromList []")

-- | Parallelism granularity.
data Options = Options {
    granularity :: !Int
  } deriving (Eq, Show)

-- | An ensemble of particles.  A Markov chain is defined over the entire
--   ensemble, rather than individual particles.
type Ensemble = Vector Particle

type Particle = U.Vector Double

type Density  = Particle -> Double

-- | The flat-mcmc transition operator.  Run a Markov chain with it by providing
--   an initial location (origin), a generator (gen), and using the usual
--   facilities from 'Control.Monad.State':
--
--   > let chain = replicateM 5000 flat `evalStateT` origin
--   > trace <- sample chain gen
--
flat :: PrimMonad m => StateT Chain (Prob m) Chain
flat = flatGranular $ Options 1

-- | The flat-mcmc transition operator with custom parallelism granularity.
flatGranular :: PrimMonad m => Options -> StateT Chain (Prob m) Chain
flatGranular (Options gran) = do
  Chain target e nAccept epochs <- get
  let n        = truncate (fromIntegral (V.length e) / 2)
      (e0, e1) = (V.slice 0 n &&& V.slice n n) e

  (e2, nAccept0) <- lift $ executeMoves target gran e0 e1
  (e3, nAccept1) <- lift $ executeMoves target gran e1 e2

  put $! Chain {
      logObjective = target
    , ensemble     = V.concat [e2, e3]
    , accepts      = nAccept + nAccept0 + nAccept1
    , iterations   = succ epochs
    }

  get

parMapChunk :: NFData b => Int -> (a -> b) -> [a] -> Par [b]
parMapChunk n f xs = concat <$> parMap (map f) (chunk n xs) where
  chunk _ [] = []
  chunk m ys =
    let (as, bs) = splitAt m ys
    in  as : chunk m bs

symmetric :: PrimMonad m => Prob m Double
symmetric = transform <$> uniform where
  transform z = 0.5 * (z + 1) ^ (2 :: Int)

stretch :: Particle -> Particle -> Double -> Particle
stretch particle altParticle z =
  U.zipWith (+) (U.map (* z) particle) (U.map (* (1 - z)) altParticle)

acceptProb :: Density -> Particle -> Particle -> Double -> Double
acceptProb target particle proposal z =
    target proposal
  - target particle
  + log z * (fromIntegral (U.length particle) - 1)

move :: Density -> Particle -> Particle -> Double -> Double -> (Particle, Int)
move target particle altParticle z zc =
  let proposal = stretch particle altParticle z
      pAccept  = acceptProb target particle proposal z
  in  if   zc <= min 0 pAccept
      then (proposal, 1) -- move and count moves made
      else (particle, 0)

executeMoves
  :: PrimMonad m
  => Density
  -> Int
  -> Ensemble
  -> Ensemble
  -> Prob m (Ensemble, Int)
executeMoves target gran e0 e1 = do
  let n = truncate $ fromIntegral (V.length e0 + V.length e1) / 2
  zs     <- replicateM n symmetric
  zcs    <- replicateM n $ log <$> uniform
  others <- replicateM n $ uniformR (0, n - 1)

  let particle j    = e0 `V.unsafeIndex` j
      altParticle j = e1 `V.unsafeIndex` (others !! j)

      moves = runPar $ parMapChunk gran
        (\(j, z, zc) -> move target (particle j) (altParticle j) z zc)
        (zip3 [0..n - 1] zs zcs)

  return $! (V.fromList . map fst &&& sum . map snd) moves

