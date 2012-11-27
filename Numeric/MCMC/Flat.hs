{-# OPTIONS_GHC -Wall #-}

module Numeric.MCMC.Flat (
            MarkovChain(..), Options(..), Ensemble
          , runChain, readInits, serializeToStdout, toVector
          ) where

import Control.Pipe
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Primitive
import System.Random.MWC
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as U
import Control.Monad.Par                    (NFData)
import Control.Monad.Par.Scheds.Direct
import Control.Monad.Par.Combinator
import GHC.Float

-- | Parallel map with a specified granularity.
parMapChunk :: NFData b => Int -> (a -> b) -> [a] -> Par [b]
parMapChunk n f xs = do
    xss <- parMap (map f) (chunk n xs)
    return (concat xss)
  where chunk _ [] = []
        chunk m ys = let (as, bs) = splitAt m ys
                     in  as : chunk m bs

-- | State of the Markov chain.  Current ensemble position is held in 'theta',
--   while 'accepts' counts the number of proposals accepted.
data MarkovChain = MarkovChain { ensemble :: Ensemble      
                               , accepts  :: {-# UNPACK #-} !Int }

-- | Display the current state.  This will be very slow and should be replaced.
instance Show MarkovChain where
    show config = filter (`notElem` "[]") $ unlines $ 
        map (show . map double2Float) (V.toList (ensemble config))

-- | Options for the chain.  The target (expected to be a log density), as
--   well as the size of the ensemble.  The size should be an even number.  Also
--   holds the specified parallel granularity as 'csize'.
data Options = Options { _nEpochs    :: {-# UNPACK #-} !Int  
                       , _burnIn     :: {-# UNPACK #-} !Int
                       , _printEvery :: {-# UNPACK #-} !Int
                       , _csize      :: {-# UNPACK #-} !Int } 

-- | An ensemble of particles.
type Ensemble = V.Vector [Double]

-- | A result with this type has a view of the chain's options.
type ViewsOptions = ReaderT Options

-- | Generate a random value from a distribution having the property that 
--   g(1/z) = zg(z).
symmetricVariate :: PrimMonad m => Gen (PrimState m) -> m Double
symmetricVariate g = do
    z <- uniformR (0 :: Double, 1 :: Double) g
    return $! 0.5*(z + 1)^(2 :: Int)
{-# INLINE symmetricVariate #-}

-- | The result of a single-particle Metropolis accept/reject step.  This 
--   compares a particle's state to a perturbation made by an affine 
--   transformation based on a complementary particle.  Non-monadic to 
--   more easily be used in the Par monad.
metropolisResult :: [Double] -> [Double] -- Target and alternate particles
                 -> Double -> Double     -- z ~ g(z) and zc ~ rand
                 -> ([Double] -> Double) -- Target function
                 -> ([Double], Int)      -- Result and accept counter
metropolisResult w0 w1 z zc target = 
    let val      =   target proposal - target w0 
                   + (fromIntegral (length w0) - 1) * log z
        proposal = zipWith (+) (map (*z) w0) (map (*(1-z)) w1) 
    in  if zc <= min 1 (exp val) then (proposal, 1) else (w0, 0)
{-# INLINE metropolisResult #-}

-- | Execute Metropolis steps on the particles of a sub-ensemble by
--   perturbing them with affine transformations based on particles
--   in a complementary ensemble, in parallel.
executeMoves :: PrimMonad m
             => ([Double] -> Double)           -- Target to sample
             -> Ensemble                       -- Target sub-ensemble
             -> Ensemble                       -- Complementary sub-ensemble
             -> Int                            -- Size of the sub-ensembles
             -> Gen (PrimState m)              -- MWC PRNG
             -> ViewsOptions m (Ensemble, Int) -- Updated ensemble/# of accepts
executeMoves t e0 e1 n g = do
    Options _ _ _ csize <- ask

    zs  <- replicateM n (lift $ symmetricVariate g)
    zcs <- replicateM n (lift $ uniformR (0 :: Double, 1 :: Double) g)
    js  <- liftM U.fromList (replicateM n (lift $ uniformR (1:: Int, n) g))

    let w0 k    = e0 `V.unsafeIndex` (k - 1) 
        w1 k ks = e1 `V.unsafeIndex` ((ks `U.unsafeIndex` (k - 1)) - 1) 

        result  = runPar $ parMapChunk csize 
                     (\(k, z, zc) -> metropolisResult (w0 k) (w1 k js) z zc t) 
                         (zip3 [1..n] zs zcs)
        (newstate, nacc) = (V.fromList . map fst &&& sum . map snd) result

    return (newstate, nacc)
{-# INLINE executeMoves #-}

-- | Perform a Metropolis accept/reject step on the ensemble by
--   perturbing each element and accepting/rejecting the perturbation in
--   parallel.
metropolisStep :: PrimMonad m
               => ([Double] -> Double)          -- Target to sample
               -> MarkovChain                   -- State of the Markov chain
               -> Gen (PrimState m)             -- MWC PRNG
               -> ViewsOptions m MarkovChain                 -- Updated sub-ensemble
metropolisStep t state g = do
    let n0        = truncate (fromIntegral n / (2 :: Double)) :: Int
        (e, nacc) = (ensemble &&& accepts) state
        (e0, e1)  = (V.slice (0 :: Int) n0 &&& V.slice n0 n0) e
        n         = V.length e
 
    -- Update each sub-ensemble 
    result0 <- executeMoves t e0 e1            n0 g
    result1 <- executeMoves t e1 (fst result0) n0 g

    return $! 
      MarkovChain (V.concat $ map fst [result0, result1]) 
                  (nacc + snd result0 + snd result1)
{-# INLINE metropolisStep #-}

-- | Diffuse through states.
observe  :: PrimMonad m
         => Options                                       -- ^ Options of the Markov chain
         -> MarkovChain                                   -- ^ Initial state of the Markov chain
         -> Gen (PrimState m)                             -- ^ MWC PRNG
         -> Pipe ([Double] -> Double) MarkovChain m r     -- ^ End state of the Markov chain, wrapped in IO
observe opts state g = forever $ do
    target <- await
    r      <- lift $ runReaderT (metropolisStep target state g) opts
    yield r
    observe opts r g

-- | Run the Markov chain.
runChain :: PrimMonad m 
         => ([Double] -> Double) 
         -> Options 
         -> MarkovChain 
         -> Gen (PrimState m) 
         -> Producer MarkovChain m r 
runChain target opts initState g = 
    forever (yield target) >+> observe opts initState g 

-- | Yield something to stdout via a simple print..
serializeToStdout :: Show a => Consumer a IO ()
serializeToStdout = forever $ await >>= lift . print

-- | Take n of something and store them in a vector.
toVector :: (U.Unbox a, Monad m) => Int -> Consumer a m (U.Vector a)
toVector n = U.replicateM n await

-- | A convenience function to read and parse ensemble inits from disk.  
--   Assumes a text file with one particle per line, where each particle
--   element is separated by whitespace.
readInits :: FilePath -> IO Ensemble
readInits p = fmap (V.fromList . map (map read . words) . lines) (readFile p)
{-# INLINE readInits #-}

