{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.MCMC.Flat
import Data.Vector (Vector)
import qualified Data.Vector as V (toList, fromList)

rosenbrock :: Vector Double -> Double
rosenbrock xs = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2) where
  [x0, x1] = V.toList xs

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 10 ensemble rosenbrock where
  ensemble = V.fromList [
      V.fromList [negate 1.0 :: Double, negate 1.0]
    , V.fromList [negate 1.0, 1.0]
    , V.fromList [1.0, negate 1.0]
    , V.fromList [1.0, 1.0]
    ]

-- ensemble = V.fromList [ V.fromList [negate 1.0 :: Double, negate 1.0], V.fromList [negate 1.0, 1.0], V.fromList [1.0, negate 1.0], V.fromList [1.0, 1.0]]
--
-- rosenbrock xs = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2) where [x0, x1] = V.toList xs
