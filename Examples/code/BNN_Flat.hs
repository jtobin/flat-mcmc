import System.IO
import System.Exit
import System.Environment
import System.Random.MWC
import Control.Monad
import Numeric.MCMC.Flat
import qualified Data.Vector as V

target :: [Double] -> Double
target [x0, x1] = -0.5*(x0^2 * x1^2 + x0^2 + x1^2 - 8*x0 - 8*x1)
{-# INLINE target #-}

main = do
    args  <- getArgs 
    when (args == []) $ do
        putStrLn  "(flat-mcmc) Bivariate non-normal density                    "
        putStrLn  "Usage: ./BNN_Flat <numSteps> <inits>                        " 
        putStrLn  "                                                            "
        putStrLn  "numSteps         : Number of Markov chain iterations to run."
        putStrLn  "inits            : Filepath containing points at which to   "
        putStrLn  "                   initialize the ensemble.                 "
        exitSuccess

    inits <- readInits (args !! 1)

    let nepochs    = read (head args) :: Int
        opts       = Options { _size      = V.length inits
                             , _nEpochs   = nepochs
                             , _burnIn    = 0
                             , _thinEvery = 1
                             , _csize     = 30               }

        initState  = MarkovChain inits 0

    g       <- create
    void $ runChain target opts initState g

