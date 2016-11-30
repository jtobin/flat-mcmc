-- |
-- Module: Data.Vector.Extended
-- Copyright: (c) 2016 Jared Tobin
-- License: MIT
--
-- Maintainer: Jared Tobin <jared@jtobin.ca>
-- Stability: unstable
-- Portability: ghc

module Data.Vector.Extended (
    ensemble
  , particle
  ) where

import qualified Data.Vector as V (fromList, Vector)
import qualified Data.Vector.Unboxed as U (fromList, Vector)

-- | A type-specialized alias for Data.Vector.fromList.
--
--   Use this to create ensembles from lists of particles.
ensemble :: [U.Vector Double] -> V.Vector (U.Vector Double)
ensemble = V.fromList

-- | A type-specialized alias for Data.Vector.Unboxed.fromList
--
--   Use this to create particles from lists of doubles.
particle :: [Double] -> U.Vector Double
particle = U.fromList

