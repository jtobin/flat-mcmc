import System.IO
import System.Exit
import System.Environment
import System.Random.MWC
import Control.Monad
import Numeric.MCMC.Flat
import qualified Data.Vector as V

target :: [Double] -> Double
target [x0, x1] = (-1)*((x0 + 2*x1 -7)^2 + (2*x0 + x1 - 5)^2)
{-# INLINE target #-}

main = do
    args  <- getArgs 
    when (args == []) $ do
        putStrLn  "(flat-mcmc) Booth density                                   "
        putStrLn  "Usage: ./Booth_Flat <numSteps> <inits>                      " 
        putStrLn  "                                                            "
        putStrLn  "numSteps         : Number of Markov chain iterations to run."
        putStrLn  "inits            : Filepath containing points at which to   "
        putStrLn  "                   initialize the ensemble.                 "
        exitSuccess

    inits <- readInits (args !! 1)

    let nepochs = read (head args) :: Int
        params  = Options     target (V.length inits) 30
        config  = MarkovChain inits 0

    g       <- create
    results <- runChain params nepochs 1 config g

    hPutStrLn stderr $ 
        let nAcc  = accepts results
            total = nepochs * V.length inits * length (V.head inits)
        in  show nAcc ++ " / " ++ show total ++ " (" ++ 
              show ((fromIntegral nAcc / fromIntegral total) :: Float) ++ 
              ") proposals accepted"



