import System.IO
import System.Exit
import System.Environment
import System.Random.MWC
import Control.Monad
import Numeric.MCMC.Flat
import qualified Data.Vector as V

target :: [Double] -> Double
target xs = go 0 0 xs 
  where go t0 t1 []         = (-t0 / (2*h)) - (0.5*h*t1)
        go t0 t1 (u:us:uss) = go (t0 + (us - u)^2) (t1 + v (us + u)) uss
        h   = 1 / fromIntegral (length xs)
        v x = (1 - x^2)^2
{-# INLINE target #-}

main = do
    args  <- getArgs 
    when (args == []) $ do
        putStrLn  "(flat-mcmc) Stochastic partial differential equation        "
        putStrLn  "Usage: ./SPDE_Flat <numSteps> <inits> <thinEvery> <burnIn> <granularity>         " 
        putStrLn  "                                                            "
        putStrLn  "numSteps         : Number of Markov chain iterations to run."
        putStrLn  "inits            : Filepath containing points at which to   "
        putStrLn  "                   initialize the ensemble.                 "
        putStrLn  "thinEvery        : Print every n^th iteration.              "
        putStrLn  "burnIn           : Number of burn-in steps.                 "
        putStrLn  "granularity      : Parallel granularity (smaller is finer). "
        exitSuccess

    inits <- readInits (args !! 1)

    let nepochs   = read (head args) :: Int
        thinEvery = read (args !! 2) :: Int
        burnIn    = read (args !! 3) :: Int
        gran      = read (args !! 4) :: Int

        opts       = Options { _size      = V.length inits
                             , _nEpochs   = nepochs
                             , _burnIn    = burnIn
                             , _thinEvery = thinEvery
                             , _csize     = gran             }

        initState  = MarkovChain inits 0

    g       <- create
    void $ runChain target opts initState g
