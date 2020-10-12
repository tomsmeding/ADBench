{-# LANGUAGE LambdaCase #-}
module Main where

import System.Environment
import System.FilePath

import GMMIO
import GMM
import Timer


writeTimes :: FilePath -> (Double, Double) -> IO ()
writeTimes fpath (timeFunc, timeJac) =
    writeFile fpath (unwords [show timeFunc, show timeJac] ++ "\n")

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
                outJPath = outDir </> testId ++ "_J_Accelerate" <.> "txt"
                outTimesPath = outDir </> testId ++ "_times_Accelerate" <.> "txt"

            input <- readInstance inPath replicatePoint
            timeImport <- timer1WHNF input  -- WHNF is sufficient because GMMIn has strict fields
            putStrLn $ "Importing took " ++ show timeImport ++ " seconds"

            (timeFunc, output1) <- timer gmmObjective input nrunsF timeLimit
            print output1
            putStrLn $ "Time taken: " ++ show timeFunc

            timeJac <- if nrunsJ > 0
                then do
                    (timeJac, output) <- timer gmmObjectiveGrad input nrunsJ timeLimit
                    writeJacobian outJPath output
                    return timeJac
                else return 0

            writeTimes outTimesPath (timeFunc, timeJac)

        _ -> error "Expected 6 or 7 arguments"
  where
    parseIntArg s = case reads s of
                      (i, "") : _ -> i
                      _ -> error $ "Expected int argument, not '" ++ s ++ "'"
