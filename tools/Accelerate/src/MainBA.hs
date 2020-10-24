{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Control.DeepSeq (force)
import Data.List (tails, findIndex)
import Data.Monoid (Any(..))
import System.Environment
import System.FilePath

import BAIO
import BA
import Timer
import Types


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

main :: IO ()
main = do
    parseArgs >>= \case
        Args flags [inDir, outDir, testId, nrunsF', nrunsJ', timeLimit'] -> do
            let nrunsF = parseIntArg nrunsF'
                nrunsJ = parseIntArg nrunsJ'
                timeLimit = parseIntArg timeLimit'
                parseFlag "gpu" = Any True
                parseFlag _ = error "Expected '-gpu' or nothing as 7th argument"
                Any useGPU = foldMap parseFlag flags
                backendKind = if useGPU then GPU else CPU

            let inPath = inDir </> testId <.> "txt"
                progName = if | "Accelerate1" `isSubstr` outDir -> "Accelerate1"
                              | "AccelerateGPU" `isSubstr` outDir -> "AccelerateGPU"
                              | otherwise -> "Accelerate"
                outJPath = outDir </> testId ++ "_J_" ++ progName <.> "txt"
                outTimesPath = outDir </> testId ++ "_times_" ++ progName <.> "txt"

            input <- baReadInstance inPath
            timeImport <- timer1WHNF input  -- WHNF is sufficient because GMMIn has strict fields
            putStrLn $ "Importing took " ++ show timeImport ++ " seconds"

            let compiledFunc = baObjectiveProgram backendKind
            timeFCompile <- timer1WHNF (force compiledFunc)
            putStrLn $ "Compilation of function took " ++ show timeFCompile ++ " seconds"

            (timeFunc, output1) <- timer (baObjective compiledFunc) input nrunsF timeLimit
            print output1
            putStrLn $ "Time taken: " ++ show timeFunc

            let compiledJac = baObjectiveJacProgram backendKind
            timeGCompile <- timer1WHNF (force compiledJac)
            putStrLn $ "Compilation of gradient took " ++ show timeGCompile ++ " seconds"

            timeJac <- if nrunsJ > 0
                then do
                    (timeJac, output) <- timer (baObjectiveJac compiledJac) input nrunsJ timeLimit
                    baWriteJacobian outJPath input output
                    return timeJac
                else return 0

            writeTimes outTimesPath (timeFunc, timeJac)

        _ -> error "Expected 6 or 7 arguments"
  where
    parseIntArg s = case reads s of
                      (i, "") : _ -> i
                      _ -> error $ "Expected int argument, not '" ++ s ++ "'"
