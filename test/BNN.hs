{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.MCMC.Flat
import Data.Vector (Vector, toList, fromList)

bnn :: Vector Double -> Double
bnn xs = -0.5 * (x0 ^ 2 * x1 ^ 2 + x0 ^ 2 + x1 ^ 2 - 8 * x0 - 8 * x1) where
  [x0, x1] = toList xs

ensemble :: Ensemble
ensemble = fromList [
    fromList [negate 1.0, negate 1.0]
  , fromList [negate 1.0, 1.0]
  , fromList [1.0, negate 1.0]
  , fromList [1.0, 1.0]
  ]

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 100 ensemble bnn

