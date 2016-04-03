{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.MCMC.Flat
import Data.Vector (Vector, toList, fromList)

rosenbrock :: Vector Double -> Double
rosenbrock xs = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2) where
  [x0, x1] = toList xs

ensemble :: Ensemble
ensemble = fromList [
    fromList [negate 1.0, negate 1.0]
  , fromList [negate 1.0, 1.0]
  , fromList [1.0, negate 1.0]
  , fromList [1.0, 1.0]
  ]

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 25000 ensemble rosenbrock

