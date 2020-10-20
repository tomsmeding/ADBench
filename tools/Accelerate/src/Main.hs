{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.DeepSeq (force)
import Control.Monad (forM_)
import qualified Criterion as Cr
import Data.List (tails)
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

main :: IO ()
main = do
    -- threadDelay 20000000

    parseArgs >>= \case
        Args ["play"] indices -> do
            let benchmark descr value = do
                    tm <- timer1WHNF (force value)
                    putStrLn $ "Play " ++ descr ++ " time taken: " ++ show tm

            let arg = Play.functionArgument
            benchmark "argument" arg

            let nfunctions = length Play.functionsToTime
            let indices' | null indices = take (3 * nfunctions) (cycle [0::Int .. nfunctions - 1])
                         | otherwise = map (pred . read) indices

            -- TODO: why does the first invocation take longer?
            forM_ indices' $ \i -> do
                benchmark ("function " ++ show (i+1)) ((Play.functionsToTime !! i) arg)

        Args ["play", "criterion"] [] -> do
            let arg = Play.functionArgument
            tmArg <- timer1WHNF (force arg)
            putStrLn $ "Forcing argument took: " ++ show tmArg

            let functions = zip Play.functionsToTime [1::Int ..]
            forM_ functions $ \(func, i) -> do
                putStrLn $ "Function " ++ show i ++ ": Warming up..."
                tm <- timer1WHNF (force (func arg))
                putStrLn $ "Running function " ++ show i ++ " took: " ++ show tm

                Cr.benchmark (Cr.nf func arg)

        Args ["play", "fusion1"] [] -> do
            print Play.fusionProgram1

        Args ["play", "fusion2"] [] -> do
            print Play.fusionProgram2

        -- Args ["testgpu"] [] -> do
        --     testGPU

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
