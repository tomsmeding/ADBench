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

import Prelude (seq)
import qualified Prelude as P
import Control.DeepSeq (NFData(..))
import Data.Array.Accelerate
-- import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- import qualified Data.Array.Accelerate.LLVM.PTX as GPU
import System.IO.Unsafe (unsafePerformIO)

import GMMIO
import Playground.M1
import Playground.M2
import Playground.M3
import Playground.M4
import Playground.M4a
import Playground.M4b


emptyInstance :: GMMIn
emptyInstance =
    let dim0 = Z
        dim1 = dim0 :. (1 :: Int)
        dim2 = dim1 :. (1 :: Int)
    in GMMIn_ (fromList @_ @Float dim1 [0.0])
              (fromList @_ @Float dim2 [0.0])
              (fromList @_ @Float dim2 [0.0])
              (fromList @_ @Float dim2 [0.0])
              (fromList @_ @Float dim0 [0.0])
              (fromList @_ @Int dim0 [0])

smallInstance :: GMMIn
smallInstance = unsafePerformIO $ readInstance "../../data/gmm/1k/gmm_d2_K5.txt" False

bigInstance :: GMMIn
bigInstance = unsafePerformIO $ readInstance "../../data/gmm/1k/gmm_d32_K25.txt" False

programs :: [Acc GMMIn -> Acc (Vector Float, Matrix Float, Matrix Float)]
programs = [inputProgram, simplified, withShapeProp, withoutExpTemps, selectiveRecompute1, selectiveRecompute2]

{-# NOINLINE fusionProgram #-}
fusionProgram :: Int -> Acc (Vector Float, Matrix Float, Matrix Float)
fusionProgram i = (programs P.!! i) (use emptyInstance)

data PreparedFuncs
    = PreparedFuncs !GMMIn
                    [GMMIn -> (Vector Float, Matrix Float, Matrix Float)]

instance NFData PreparedFuncs where
    rnf (PreparedFuncs input fs) = rnf input `seq` P.foldr seq () fs

{-# NOINLINE prepare #-}
prepare :: PreparedFuncs
prepare = PreparedFuncs bigInstance [CPU.run1 p | p <- programs]

{-# NOINLINE functionsToTime #-}
functionsToTime :: [PreparedFuncs -> ()]
functionsToTime =
    [\(PreparedFuncs input fs) -> (fs P.!! i) input `seq` ()
    | i <- [0..5]]

newtype PreparedArgument = PreparedArgument GMMIn

instance NFData PreparedArgument where
    rnf (PreparedArgument input) = rnf input

prepareArgument :: PreparedArgument
prepareArgument = PreparedArgument bigInstance

{-# NOINLINE functionsToTimeUncached #-}
functionsToTimeUncached :: [PreparedArgument -> ()]
functionsToTimeUncached =
    [\(PreparedArgument input) -> CPU.run ((programs P.!! i) (use input)) `seq` ()
    | i <- [0..5]]
