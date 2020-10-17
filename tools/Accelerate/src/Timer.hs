{-# LANGUAGE ScopedTypeVariables #-}
module Timer (
    timer, timer1WHNF
) where

import Control.DeepSeq (NFData)
import Criterion.Measurement (measure)
import Criterion.Measurement.Types (nf, nfAppIO, measTime)
import Data.IORef


-- | Run the function once with the specified argument, and return the amount
-- of wall-clock time spent.
runOnce :: NFData b => (a -> b) -> a -> IO Double
runOnce func arg = do
    (measured, _) <- measure (nf func arg) 1
    -- putStrLn ("(timer: sample " ++ show (measTime measured) ++ ")")
    return (measTime measured)

-- | Run the function once with the specified argument, and return the amount
-- of wall-clock time spent, as well as the function output.
runOnceResult :: NFData b => (a -> b) -> a -> IO (Double, b)
runOnceResult func arg = do
    -- Write the function output to an IORef within the timed lambda.
    -- This is kind of ugly, but it works.
    resultRef <- newIORef Nothing
    let func' x = do
            let output = func x
            writeIORef resultRef (Just output)
            return output
    (measured, _) <- measure (nfAppIO func' arg) 1
    -- putStrLn ("(timer: sample " ++ show (measTime measured) ++ ")")
    Just result <- readIORef resultRef
    return (measTime measured, result)

-- | Repeats calling the given function until either the specified number of
-- runs have been performed, or the total time exceeds the limit -- whichever
-- occurs sooner. Returns the amount of time per run, as well as the function
-- output (collected in the first invocation).
timer :: forall a b. NFData b => (a -> b) -> a -> Int -> Double -> IO (Double, b)
timer _    _   0     _     = error "timer: Cannot produce value if number of runs is 0"
timer func arg nruns limit = do
    (tm, output) <- runOnceResult func arg
    (actualNruns', total) <- go 1 tm
    return (total / fromIntegral (actualNruns' + 1), output)
  where
    -- Returns number of invocations and total time spent over all invocations.
    go :: Int -> Double -> IO (Int, Double)
    go runsDone total
      | runsDone < nruns, total < limit = do
          tm <- runOnce func arg
          (actualNruns, total') <- go (runsDone + 1) (total + tm)
          return (actualNruns + 1, total')
      | otherwise =
          return (0, total)

-- | Measures how long it takes to evaluate the argument to WHNF.
timer1WHNF :: a -> IO Double
timer1WHNF = runOnce (`seq` ())
