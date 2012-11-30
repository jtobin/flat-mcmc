import Control.Pipe

import System.IO
import System.Exit
import System.Environment
import System.Random.MWC
import Control.Monad
import Numeric.MCMC.Flat
import qualified Data.Vector as V

target :: [Double] -> Double
target [x0, x1] = (-1)*(5*(x1 - x0^2)^2 + 0.05*(1 - x0)^2)
target _        = error "explode"

deliver :: Int -> Consumer MarkovChain IO (V.Vector MarkovChain)
deliver n = V.replicateM n await

main = do
    args  <- getArgs 
    when (args == []) $ do
        putStrLn  "(flat-mcmc) Rosenbrock density                              "
        putStrLn  "Usage: ./Rosenbrock_Flat <numSteps> <inits>                 " 
        putStrLn  "                                                            "
        putStrLn  "numSteps         : Number of Markov chain iterations to run."
        putStrLn  "inits            : Filepath containing points at which to   "
        putStrLn  "                   initialize the ensemble.                 "
        exitSuccess

    inits <- readInits (args !! 1)

    let nepochs = read (head args) :: Int
        opts       = Options { _nEpochs    = nepochs
                             , _burnIn     = 0
                             , _printEvery = 1
                             , _csize      = 30      }
        initState  = MarkovChain inits 0

    g       <- create
    runPipe $     runChain target opts initState g >+> yieldOnly nepochs
              >+> serializeToStdout >+> forever (void await)
              -- >+> approxExpectationWith nepochs

