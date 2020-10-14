{-# LANGUAGE LambdaCase #-}
module Main where

import Control.DeepSeq (force)
import Data.List (tails)
import System.Environment
import System.FilePath

import GMMIO
import GMM
import Timer


writeTimes :: FilePath -> (Double, Double) -> IO ()
writeTimes fpath (timeFunc, timeJac) =
    writeFile fpath (unwords [show timeFunc, show timeJac] ++ "\n")

isSubstr :: String -> String -> Bool
small `isSubstr` large = any (`startsWith` small) (tails large)
  where s `startsWith` prefix = take (length prefix) s == prefix

main :: IO ()
main = do
    getArgs >>= \case
        (inDir : outDir : testId : nrunsF' : nrunsJ' : timeLimit' : rest) -> do
            let nrunsF = parseIntArg nrunsF'
                nrunsJ = parseIntArg nrunsJ'
                timeLimit = parseIntArg timeLimit'
                replicatePoint = case rest of
                   ["-rep"] -> True
                   [] -> False
                   _ -> error "Expected '-rep' or nothing as 7th argument"

            let inPath = inDir </> testId <.> "txt"
                progName = if "Accelerate1" `isSubstr` outDir then "Accelerate1" else "Accelerate"
                outJPath = outDir </> testId ++ "_J_" ++ progName <.> "txt"
                outTimesPath = outDir </> testId ++ "_times_" ++ progName <.> "txt"

            input <- readInstance inPath replicatePoint
            timeImport <- timer1WHNF input  -- WHNF is sufficient because GMMIn has strict fields
            putStrLn $ "Importing took " ++ show timeImport ++ " seconds"

            let compiledFunc = gmmObjectiveProgram input
            timeFCompile <- timer1WHNF (force compiledFunc)
            putStrLn $ "Compilation of function took " ++ show timeFCompile ++ " seconds"

            (timeFunc, output1) <- timer (gmmObjective compiledFunc) input nrunsF timeLimit
            print output1
            putStrLn $ "Time taken: " ++ show timeFunc

            let compiledGrad = gmmObjectiveGradProgram input
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
