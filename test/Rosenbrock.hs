{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.MCMC.Flat
import qualified Data.Vector.Unboxed as U (Vector, toList, fromList)
import qualified Data.Vector as V (fromList)

rosenbrock :: U.Vector Double -> Double
rosenbrock xs = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2) where
  [x0, x1] = U.toList xs

ensemble :: Ensemble
ensemble = V.fromList [
    U.fromList [negate 1.0, negate 1.0]
  , U.fromList [negate 1.0, 1.0]
  , U.fromList [1.0, negate 1.0]
  , U.fromList [1.0, 1.0]
  ]

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 100 ensemble rosenbrock

