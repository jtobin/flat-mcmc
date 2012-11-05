---
title: flat-mcmc
---
### what is this i don't even

flat-mcmc is a [Haskell](http://www.haskell.org) library for painless, efficient, general-purpose sampling from continuous distributions.  

Sampling is commonly used in Bayesian statistics/machine learning, physics, and finance to approximate difficult integrals or estimate model parameters.  Haskell is an advanced functional language emphasizing abstraction, performance, multicore support, and security.

### tell me more

Consider using a Gaussian likelihood model for some data.  A conjugate prior yields a posterior that, in parameter space, looks like this:

![](img/1DGaussian.png)

The Metropolis-Hastings algorithm is the 'go-to' general-purpose sampler, proposing global moves over both parameters simultaneously:

![](img/1DGaussian_MH.png)

Distributions like this are easy to sample from efficiently. The case is different for *anisotropic* distributions that are 'skewed' or 'stretched'. They look funny; narrow and correlated, like the Rosenbrock density on the plane:

<br>
<script src="https://gist.github.com/3865828.js?file=gistfile1.hs"></script>

![](img/Rosenbrock.png)

With some cleverness, the Rosenbrock density can be sampled independently.  One thousand independent samples look like this:

![](img/Rosenbrock_IND.png)

Conventional Markov chain samplers have trouble moving around narrow regions of the parameter space.  Take fifty thousand iterations of a Metropolis-Hastings sampler, using naive Gaussian 'bubble' proposals: 

![](img/Rosenbrock_MH.png)

Tailoring good proposals requires local knowledge of the target manifold, which can be particularly unpleasant to incorporate in high dimensions. 

[Hamiltonian Monte Carlo (HMC)](http://github.com/jtobin/hasty-hamiltonian) immediately finds regions of appreciable density, but without extensive tuning has trouble moving into the tails of the distribution:

![](img/Rosenbrock_HMC.png)

Another method requiring no tuning at all involves ensemble samplers that are [invariant to affine transformations of space](http://msp.org/camcos/2010/5-1/p04.xhtml).  In essence, they 'unstretch' the target's parameter space, allowing many particles to explore the distribution locally.  Half of the work of the Metropolis-Hastings sampler yields something like this:

![](img/Rosenbrock_AIE.png)

[Good implementations of these algorithms exist](http://danfm.ca/emcee).  Haskell yields some perks; samplers can be compiled, and it is trivial to incorporate nested parallelism for specialized performance.  flat-mcmc supports parallel evaluation of the target function via any of Haskell's available methods.  Using the Par monad, for example:

<br>
<script src="https://gist.github.com/3865601.js?file=gistfile1.hs"></script>
<br>

flat-mcmc also supports **nested** parallelism, in that these function evaluations will also be executed in parallel on each iteration if compiled with GHC's threaded runtime.  Performance is quite good:

<br>
<script src="https://gist.github.com/3865854.js?file=gistfile1.txt"></script>
<br>

### join the club

Pull requests welcome.  If you find this work useful in your research, please cite it as 

*   Tobin, J. (2012) flat-mcmc: A library for efficient, general purpose sampling.  [jtobin.github.com/flat-mcmc](jtobin.github.com/flat-mcmc)

flat-mcmc is written and maintained by [Jared Tobin](http://jtobin.ca).  Page generated with [Hakyll](http://jaspervdj.be/hakyll/).

