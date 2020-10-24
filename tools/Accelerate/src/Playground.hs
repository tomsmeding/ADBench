{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Playground (
    prepare,
    functionsToTime,

    prepareArgument,
    functionsToTimeUncached,

    fusionProgram,
) where

import Control.DeepSeq (NFData(..))
import Data.Array.Accelerate (Z(..), (:.)(..), Acc, Vector, Matrix)
import qualified Data.Array.Accelerate as A
-- import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- import qualified Data.Array.Accelerate.LLVM.PTX as GPU
import System.IO.Unsafe (unsafePerformIO)

import GMMIO
import Playground.M1
import Playground.M1N
import Playground.M2
import Playground.M3
import Playground.M4
import Playground.M4a
import Playground.M4b
import Playground.M4N


emptyInstance :: GMMIn
emptyInstance =
    let dim0 = Z
        dim1 = dim0 :. (1 :: Int)
        dim2 = dim1 :. (1 :: Int)
    in GMMIn_ (A.fromList @_ @Float dim1 [0.0])
              (A.fromList @_ @Float dim2 [0.0])
              (A.fromList @_ @Float dim2 [0.0])
              (A.fromList @_ @Float dim2 [0.0])
              (A.fromList @_ @Float dim0 [0.0])
              (A.fromList @_ @Int dim0 [0])

smallInstance :: GMMIn
smallInstance = unsafePerformIO $ gmmReadInstance "../../data/gmm/1k/gmm_d2_K5.txt" False

bigInstance :: GMMIn
bigInstance = unsafePerformIO $ gmmReadInstance "../../data/gmm/1k/gmm_d32_K25.txt" False

programs :: [(String, Acc GMMIn -> Acc (Vector Float, Matrix Float, Matrix Float))]
programs = [("M1", inputProgram)
           ,("M1N", withDefaultBackpermute)
           ,("M2", simplified)
           ,("M3", withShapeProp)
           ,("M4", withoutExpTemps)
           ,("M4a", selectiveRecompute1)
           ,("M4b", selectiveRecompute2)
           ,("M4N", withoutExpTempsN)]

{-# NOINLINE fusionProgram #-}
fusionProgram :: Int -> Acc (Vector Float, Matrix Float, Matrix Float)
fusionProgram i = snd (programs !! i) (A.use emptyInstance)

data PreparedFuncs
    = PreparedFuncs !GMMIn
                    [GMMIn -> (Vector Float, Matrix Float, Matrix Float)]

instance NFData PreparedFuncs where
    rnf (PreparedFuncs input fs) = rnf input `seq` foldr seq () fs

{-# NOINLINE prepare #-}
prepare :: PreparedFuncs
prepare = PreparedFuncs bigInstance [CPU.run1 p | (_, p) <- programs]

{-# NOINLINE functionsToTime #-}
functionsToTime :: [(String, PreparedFuncs -> ())]
functionsToTime =
    [(n, \(PreparedFuncs input fs) -> (fs !! i) input `seq` ())
    | (i, (n, _)) <- zip [0..] programs]

newtype PreparedArgument = PreparedArgument GMMIn

instance NFData PreparedArgument where
    rnf (PreparedArgument input) = rnf input

prepareArgument :: PreparedArgument
prepareArgument = PreparedArgument bigInstance

{-# NOINLINE functionsToTimeUncached #-}
functionsToTimeUncached :: [(String, PreparedArgument -> ())]
functionsToTimeUncached =
    [(n, \(PreparedArgument input) -> CPU.run (p (A.use input)) `seq` ())
    | (n, p) <- programs]
