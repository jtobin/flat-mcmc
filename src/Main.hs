{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Numeric.MCMC.Flat
import qualified Data.Vector.Unboxed as U (toList)

bnn :: Particle -> Double
bnn xs = -0.5 * (x0 ^ 2 * x1 ^ 2 + x0 ^ 2 + x1 ^ 2 - 8 * x0 - 8 * x1) where
  [x0, x1] = U.toList xs

origin :: Ensemble
origin = ensemble [
    particle [negate 1.0, negate 1.0]
  , particle [negate 1.0, 1.0]
  , particle [1.0, negate 1.0]
  , particle [1.0, 1.0]
  ]

main :: IO ()
main = withSystemRandom . asGenIO $ mcmc 10000 origin bnn

