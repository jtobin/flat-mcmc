{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.MCMC.Flat
import qualified Data.Vector.Unboxed as U (Vector, toList, fromList)
import qualified Data.Vector as V (fromList)

bnn :: Particle -> Double
bnn xs = -0.5 * (x0 ^ 2 * x1 ^ 2 + x0 ^ 2 + x1 ^ 2 - 8 * x0 - 8 * x1) where
  [x0, x1] = U.toList xs

ensemble :: Ensemble
ensemble = V.fromList [
    U.fromList [negate 1.0, negate 1.0]
  , U.fromList [negate 1.0, 1.0]
  , U.fromList [1.0, negate 1.0]
  , U.fromList [1.0, 1.0]
  ]

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 1000 ensemble bnn

