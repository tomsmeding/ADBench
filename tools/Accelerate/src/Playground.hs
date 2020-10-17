{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Playground (
    functionArgument,
    functionsToTime,
    fusionProgram1, fusionProgram2
) where

import Prelude (seq)
import Control.DeepSeq (NFData(..))
import Data.Array.Accelerate
-- import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.LLVM.Native as CPU
import System.IO.Unsafe (unsafePerformIO)

import GMMIO
import Playground.M1
import Playground.M2
import Playground.M3
import Playground.M4


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

{-# NOINLINE fusionProgram1 #-}
fusionProgram1 :: Acc (Vector Float, Matrix Float, Matrix Float)
fusionProgram1 = simplified (use emptyInstance)

{-# NOINLINE fusionProgram2 #-}
fusionProgram2 :: Acc (Vector Float, Matrix Float, Matrix Float)
fusionProgram2 = withShapeProp (use emptyInstance)

data FunctionArgument
    = FunctionArgument !GMMIn
                       !(Acc GMMIn -> Acc (Vector Float, Matrix Float, Matrix Float))
                       !(Acc GMMIn -> Acc (Vector Float, Matrix Float, Matrix Float))
                       !(Acc GMMIn -> Acc (Vector Float, Matrix Float, Matrix Float))
                       !(Acc GMMIn -> Acc (Vector Float, Matrix Float, Matrix Float))

instance NFData FunctionArgument where
    rnf (FunctionArgument input f1 f2 f3 f4) = rnf input `seq` f1 `seq` f2 `seq` f3 `seq` f4 `seq` ()

{-# NOINLINE functionArgument #-}
functionArgument :: FunctionArgument
functionArgument = FunctionArgument bigInstance inputProgram simplified withShapeProp withoutExpTemps

{-# NOINLINE functionsToTime #-}
functionsToTime :: [FunctionArgument -> ()]
functionsToTime =
    [\(FunctionArgument input f1 _ _ _) -> CPU.run (f1 (use input)) `seq` ()
    ,\(FunctionArgument input _ f2 _ _) -> CPU.run (f2 (use input)) `seq` ()
    ,\(FunctionArgument input _ _ f3 _) -> CPU.run (f3 (use input)) `seq` ()
    ,\(FunctionArgument input _ _ _ f4) -> CPU.run (f4 (use input)) `seq` ()]
