{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Numeric.MCMC.Flat
import System.Random.MWC.Probability

lRosenbrock :: Density
lRosenbrock xs =
  let [x0, x1] = U.toList xs
  in  (-1) * (5 * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

defaultEnsemble :: Ensemble
defaultEnsemble = V.fromList $ map U.fromList 
  [[0.1, 0.5], [0.8, 0.1], [1.0, 0.2], [0.9, 0.8], [-0.2, 0.3], [-0.1, 0.9]]

opts :: Options
opts = Options 10

origin :: Chain
origin = Chain {
    logObjective = lRosenbrock
  , ensemble     = defaultEnsemble
  , iterations   = 0
  , accepts      = 0
  } 

-- cabal test --show-details=streaming
main :: IO ()
main = withSystemRandom . asGenIO $ \g -> do
  trace <- sample (replicateM 5000 (flat opts) `evalStateT` origin) g
  print trace

