{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.MCMC.Flat
import qualified Data.Vector.Unboxed as U (unsafeIndex)

rosenbrock :: Particle -> Double
rosenbrock xs = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2) where
  x0 = U.unsafeIndex xs 0
  x1 = U.unsafeIndex xs 1

origin :: Ensemble
origin = ensemble [
    particle [negate 1.0, negate 1.0]
  , particle [negate 1.0, 1.0]
  , particle [1.0, negate 1.0]
  , particle [1.0, 1.0]
  ]

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 100 origin rosenbrock

