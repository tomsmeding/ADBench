{-# LANGUAGE LambdaCase #-}
module Main where

-- import Control.Concurrent (threadDelay)
import Control.DeepSeq (force)
import Control.Monad (forM, forM_)
import qualified Criterion as Cr
import Data.List (tails, findIndex)
import Data.Monoid (Any(..))
import System.Environment
import System.FilePath

import GMMIO
import GMM
import Timer
import Types
import qualified Playground as Play

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.LLVM.PTX as GPU


testGPU :: IO ()
testGPU = do
    let prog = A.sum (A.generate (A.I1 1000000) (\(A.I1 i) -> A.toFloating i :: A.Exp Float))
    print $ GPU.run prog
    inst <- readInstance "../../data/gmm/1k/gmm_d2_K5.txt" False
    print $ gmmObjective (gmmObjectiveProgram GPU inst) inst


data Stats a = Stats { statsMean :: a, statsStddev :: a, statsStderr :: a }
  deriving (Show)

prettyStats :: (Show a, Floating a) => Stats a -> String
prettyStats (Stats mu sdev serr) = "Mean: " ++ show mu ++ " ± " ++ show serr ++ "; stddev: " ++ show sdev

computeStats :: Floating a => [a] -> Stats a
computeStats l =
    let n' = fromIntegral (length l)
        mu = sum l / n'
        sdev = sum (map (\x -> (x - mu) * (x - mu)) l) / (n' - 1)
        serr = sdev / sqrt n'
    in Stats mu sdev serr


writeTimes :: FilePath -> (Double, Double) -> IO ()
writeTimes fpath (timeFunc, timeJac) =
    writeFile fpath (unwords [show timeFunc, show timeJac] ++ "\n")

isSubstr :: String -> String -> Bool
small `isSubstr` large = any (`startsWith` small) (tails large)
  where s `startsWith` prefix = take (length prefix) s == prefix

-- Args flags bare-args
data Args = Args [String] [String]
  deriving (Show)

instance Semigroup Args where
    Args a b <> Args a' b' = Args (a <> a') (b <> b')

instance Monoid Args where
    mempty = Args mempty mempty

parseArgs :: IO Args
parseArgs = foldMap parseArg <$> getArgs
  where parseArg ('-':flag) = Args [flag] []
        parseArg arg = Args [] [arg]

parseIndexMultiple :: String -> [Int]
parseIndexMultiple input
  | Just idx <- findIndex (== 'x') input
  = replicate (read (take idx input)) (read (drop (idx + 1) input) - 1)
  | otherwise
  = [read input - 1]

entryPlayFunctions :: [String] -> IO ()
entryPlayFunctions indices = do
    let arg = Play.prepare
    argTm <- timer1WHNF (force arg)
    putStrLn $ "Play argument + compilation time taken: " ++ show argTm

    let nfunctions = length Play.functionsToTime
    let indices' | null indices = take (3 * nfunctions) (cycle [0::Int .. nfunctions - 1])
                 | otherwise = concatMap parseIndexMultiple indices

    -- TODO: why do the first invocations take longer?
    let warmupIndex = if null indices' then 0 else head indices'
        performWarmup times idx = do
            warmupTm <- timer1WHNF (force ((Play.functionsToTime !! idx) arg))
            putStrLn $ "Play warmup(" ++ show (times::Int) ++ ") function " ++ show (warmupIndex+1) ++ " time taken: " ++ show warmupTm

    performWarmup 1 warmupIndex
    performWarmup 2 warmupIndex

    stats <- fmap computeStats . forM indices' $ \i -> do
        tm <- timer1WHNF (force ((Play.functionsToTime !! i) arg))
        putStrLn ("Play function " ++ show (i+1) ++ " time taken: " ++ show tm)
        return tm
    putStrLn (prettyStats stats)

entryPlayFunctionsNoWarmup :: [String] -> IO ()
entryPlayFunctionsNoWarmup indices = do
    let arg = Play.prepareArgument
    argTm <- timer1WHNF (force arg)
    putStrLn $ "Play argument time taken: " ++ show argTm

    let nfunctions = length Play.functionsToTime
    let indices' | null indices = take (3 * nfunctions) (cycle [0::Int .. nfunctions - 1])
                 | otherwise = concatMap parseIndexMultiple indices

    putStrLn "Not caching compilation results!"

    stats <- fmap computeStats . forM indices' $ \i -> do
        tm <- timer1WHNF (force ((Play.functionsToTimeUncached !! i) arg))
        putStrLn ("Play function " ++ show (i+1) ++ " time taken: " ++ show tm)
        return tm
    putStrLn (prettyStats stats)

entryPlayCriterion :: IO ()
entryPlayCriterion = do
    let arg = Play.prepare
    tmArg <- timer1WHNF (force arg)
    putStrLn $ "Forcing argument + compilation took: " ++ show tmArg

    let functions = zip Play.functionsToTime [1::Int ..]
    forM_ functions $ \(func, i) -> do
        putStrLn $ "Function " ++ show i ++ ": Warming up..."
        tm <- timer1WHNF (force (func arg))
        putStrLn $ "Running function " ++ show i ++ " took: " ++ show tm

        Cr.benchmark (Cr.nf func arg)

main :: IO ()
main = do
    -- threadDelay 20000000

    parseArgs >>= \case
        Args ["play"] indices -> entryPlayFunctions indices
        Args ["play", "nowarm"] indices -> entryPlayFunctionsNoWarmup indices

        Args ["play", "criterion"] [] -> entryPlayCriterion

        Args ["play", "fusion"] [index] -> do
            print (Play.fusionProgram (read index - 1))

        Args ["testgpu"] [] -> testGPU

        Args flags [inDir, outDir, testId, nrunsF', nrunsJ', timeLimit'] -> do
            let nrunsF = parseIntArg nrunsF'
                nrunsJ = parseIntArg nrunsJ'
                timeLimit = parseIntArg timeLimit'
                parseFlag "rep" = (Any True, Any False)
                parseFlag "gpu" = (Any False, Any True)
                parseFlag _ = error "Expected '-rep', '-gpu' or nothing as 7th argument"
                (Any replicatePoint, Any useGPU) = foldMap parseFlag flags
                backendKind = if useGPU then GPU else CPU

            let inPath = inDir </> testId <.> "txt"
                progName = if "Accelerate1" `isSubstr` outDir then "Accelerate1" else "Accelerate"
                outJPath = outDir </> testId ++ "_J_" ++ progName <.> "txt"
                outTimesPath = outDir </> testId ++ "_times_" ++ progName <.> "txt"

            input <- readInstance inPath replicatePoint
            timeImport <- timer1WHNF input  -- WHNF is sufficient because GMMIn has strict fields
            putStrLn $ "Importing took " ++ show timeImport ++ " seconds"

            let compiledFunc = gmmObjectiveProgram backendKind input
            timeFCompile <- timer1WHNF (force compiledFunc)
            putStrLn $ "Compilation of function took " ++ show timeFCompile ++ " seconds"

            (timeFunc, output1) <- timer (gmmObjective compiledFunc) input nrunsF timeLimit
            print output1
            putStrLn $ "Time taken: " ++ show timeFunc

            let compiledGrad = gmmObjectiveGradProgram backendKind input
            timeGCompile <- timer1WHNF (force compiledGrad)
            putStrLn $ "Compilation of gradient took " ++ show timeGCompile ++ " seconds"

            timeJac <- if nrunsJ > 0
                then do
                    (timeJac, output) <- timer (gmmObjectiveGrad compiledGrad) input nrunsJ timeLimit
                    writeJacobian outJPath output
                    return timeJac
                else return 0

            writeTimes outTimesPath (timeFunc, timeJac)

        _ -> error "Expected 6 or 7 arguments"
  where
    parseIntArg s = case reads s of
                      (i, "") : _ -> i
                      _ -> error $ "Expected int argument, not '" ++ s ++ "'"
