# flat-mcmc

[![Hackage Version](https://img.shields.io/hackage/v/flat-mcmc.svg)](http://hackage.haskell.org/package/flat-mcmc)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jtobin/flat-mcmc/blob/master/LICENSE)

*flat-mcmc* is a Haskell library for painless, efficient, general-purpose
sampling from continuous distributions.

*flat-mcmc* uses an ensemble sampler that is invariant to affine
transformations of space.  It wanders a target probability distribution's
parameter space as if it had been "flattened" or "unstretched" in some sense,
allowing many particles to explore it locally and in parallel.

In general this sampler is useful when you want decent performance without
dealing with any tuning parameters or local proposal distributions.  Check out
the paper describing the algorithm
[here](http://msp.org/camcos/2010/5-1/camcos-v5-n1-p04-p.pdf), and a paper on
some potential limitations [here](http://arxiv.org/abs/1509.02230), authored
by my friends David Huijser and [Brendon
Brewer](https://www.stat.auckland.ac.nz/~brewer/).  There is also also a robust
Python implementation [here](http://dan.iel.fm/emcee/current/) authored by [Dan
Foreman-Mackey](http://dan.iel.fm), a very nice dude who I once moved some
furniture with.

*flat-mcmc* exports an 'mcmc' function that prints a trace to stdout, as well
as a 'flat' transition operator that can be used more generally.

``` haskell
import Numeric.MCMC.Flat
import qualified Data.Vector.Unboxed as U (unsafeIndex)

rosenbrock :: Particle -> Double
rosenbrock xs = negate (5  * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2) where
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
main = withSystemRandom . asGenIO $ mcmc 12500 origin rosenbrock
```

![trace](http://jtobin.ca/flat-mcmc/img/Rosenbrock_AIE.png)

