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
        params = Options target (V.length inits) 20
        config = MarkovChain inits 0

    g       <- create
    results <- runChain params nepochs 0 1 config g

    hPutStrLn stderr $ 
        let nAcc  = accepts results
            total = nepochs * V.length inits * length (V.head inits)
        in  show nAcc ++ " / " ++ show total ++ " (" ++ 
              show ((fromIntegral nAcc / fromIntegral total) :: Float) ++ 
              ") proposals accepted"

