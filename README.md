# flat-mcmc [![Build Status](https://secure.travis-ci.org/jtobin/flat-mcmc.png)](http://travis-ci.org/jtobin/flat-mcmc)

*flat-mcmc* is a Haskell library for painless, efficient, general-purpose
sampling from continuous distributions.

*flat-mcmc* uses an ensemble sampler that is invariant to affine
transformations of space.  It wanders a target probability distribution's
parameter space as if it had been "flattened" or "unstretched" in some sense,
allowing many particles to explore it locally and in parallel.

In general this sampler is useful when you want decent performance without
dealing with any tuning parameters or local proposal distributions.

*flat-mcmc* exports an 'mcmc' function that prints a trace to stdout, as well
as a 'flat' transition operator that can be used more generally.

``` haskell
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
```
