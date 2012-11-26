import System.IO
import System.Exit
import System.Environment
import System.Random.MWC
import Control.Monad
import Numeric.MCMC.Flat
import qualified Data.Vector as V

target :: [Double] -> Double
target [x0, x1] = (-1)*((x0*x0 + x1 - 11)^2 + (x0 + x1*x1 - 7)^2)
{-# INLINE target #-}

main = do
    args  <- getArgs 
    when (args == []) $ do
        putStrLn  "(flat-mcmc) Himmelblau density                              "
        putStrLn  "Usage: ./Himmelblau_Flat <numSteps> <burnIn> <inits>        " 
        putStrLn  "                                                            "
        putStrLn  "numSteps         : Number of Markov chain iterations to run."
        putStrLn  "burnIn           : Number of burn-in steps to perform.      "
        putStrLn  "inits            : Filepath containing points at which to   "
        putStrLn  "                   initialize the ensemble.                 "
        exitSuccess

    inits <- readInits (args !! 2)

    let nepochs = read (head args) :: Int
        burnIn  = read (args !! 1) :: Int
 
        opts       = Options { _size      = V.length inits
                             , _nEpochs   = nepochs
                             , _burnIn    = burnIn
                             , _thinEvery = 1
                             , _csize     = 30               }

        initState  = MarkovChain inits 0

    g       <- create
    void $ runChain target opts initState g

