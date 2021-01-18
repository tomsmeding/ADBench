{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module GMM (
    gmmObjective, gmmObjectiveProgram,
    gmmObjectiveGrad, gmmObjectiveGradProgram,
) where

import Prelude (id)
import qualified Prelude as P
import Control.DeepSeq (NFData)
import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- import qualified Data.Array.Accelerate.LLVM.PTX as GPU
import Numeric.SpecFunctions (logGamma)

import GMMIO
import Types


data Precomputed =
    Precomputed_ FLT  -- ^ 1/2 N D log(2 pi)
                 FLT  -- ^ K (n' D (log gamma - 1/2 log(2)) - multigammaln(1/2 n', D))
  deriving (Show, Generic)

instance Elt Precomputed

pattern Precomputed :: Exp FLT -> Exp FLT -> Exp Precomputed
pattern Precomputed c1 c2 = Pattern (c1, c2)
{-# COMPLETE Precomputed #-}

cAll :: Exp All
cAll = constant All

precompute :: GMMIn -> Precomputed
precompute input =
    let Z :. n :. d = arrayShape (gmmInX input)
        Z :. k = arrayShape (gmmInAlphas input)
        n' = P.fromIntegral n
        d' = P.fromIntegral d
        k' = P.fromIntegral k
        gamma = gmmInWisGamma input `linearIndexArray` 0
        m = gmmInWisM input `linearIndexArray` 0
        n2 = d' + P.fromIntegral m + 1
    in Precomputed_ (0.5 * n' * d' * log (2 * pi))
                    (k' * (n2 * d' * (log gamma - 0.5 * log 2) - multigammaln (0.5 * n2) d))

multigammaln :: (P.RealFrac a, P.Floating a) => a -> Int -> a
multigammaln a d =
    let logGamma' = P.realToFrac . logGamma . P.realToFrac
    in P.fromIntegral (d * (d - 1)) * 0.25 * log pi
        + P.sum [logGamma' (a - P.fromIntegral (i - 1) * 0.5) | i <- [1..d]]

logsumexp :: (Ord e, Floating e, Shape sh)
          => Acc (Array (sh :. Int) e) -> Acc (Array sh e)
logsumexp x =
    let len = indexHead (shape x)
        maxx = maximum x
        shiftedx = zipWith (-) x (replicate (lift Any ::. len) maxx)
    in zipWith (+) (map log (sum (map exp shiftedx))) maxx


sqsum :: (Num a, Shape sh) => Acc (Array (sh :. Int) a) -> Acc (Array sh a)
sqsum = sum . map (\x -> x * x)

logWishartPrior :: Exp Int
                -> Acc (Matrix FLT) -> Acc (Vector FLT) -> Acc (Matrix FLT)
                -> Acc (Scalar FLT) -> Acc (Scalar Int)
                -> Exp Precomputed
                -> Acc (Scalar FLT)
logWishartPrior k qdiags sumQs lvals wisGamma wisM (Precomputed _ c2) =
    let res1 = sum $
            zipWith (-)
                (zipWith (*) (zipWith (+) (sqsum qdiags) (sqsum lvals))
                             (replicate (I1 k) (map (\x -> 0.5 * x * x) wisGamma)))
                (zipWith (*) sumQs (replicate (I1 k) (map fromIntegral wisM)))
    in map (subtract c2) res1

objective :: Acc GMMIn -> Exp Precomputed -> Acc (Scalar FLT)
objective (GMMIn alphas means icf x wisGamma wisM) prec@(Precomputed c1 _) =
    let I2 n d = shape x
        I2 _ numQL = shape icf
        numL = numQL - d  -- numL = 1/2 d (d - 1)
        I1 k = shape alphas
        qvals = backpermute (I2 k d) id icf
        lvals = backpermute (I2 k numL) (\(I2 i j) -> I2 i (j + d)) icf
        qdiags = map exp qvals
        sumQs = sum qvals
        shiftedX = zipWith (-) (replicate (I3 cAll k cAll) x)
                               (replicate (I3 n cAll cAll) means)
        -- If we would efficiently support array indexing in the AD
        -- transformation, the 'generate' definition of lmats would be the most
        -- readable. However, since we don't, we will define it in terms of
        -- more information-retaining primitives in the 'backpermute'
        -- definition of lmats.
        lmats = if False
            then generate (I4 n k d d)
                          (\(I4 _ ki i1 i2) ->
                              let li = d * (d - 1) `div` 2 - (d-1 - i2) * (d-1 - i2 + 1) `div` 2
                                         + i1 - i2 - 1
                              in cond (i1 > i2) (lvals ! I2 ki li) 0)
            else let -- For lack of a backpermute-with-default primitive
                     lvalsWithZero = zipWith (*)
                                         (generate (I2 k (numL + 1)) (\(I2 _ j) -> cond (j == 0) 0 1))
                                         (backpermute (I2 k (numL + 1)) (\(I2 i j) -> I2 i (j + d - 1)) icf)
                 in backpermute (I4 n k d d)
                                (\(I4 _ ki i1 i2) ->
                                    let li = d * (d - 1) `div` 2 - (d-1 - i2) * (d-1 - i2 + 1) `div` 2
                                               + i1 - i2 - 1
                                    in cond (i1 > i2) (I2 ki (li + 1)) (I2 ki 0))
                                lvalsWithZero
        eqxprod = zipWith (*) (replicate (I3 n cAll cAll) qdiags) shiftedX
        lxprod = sum (zipWith (*) lmats (replicate (I4 cAll cAll d cAll) shiftedX))
        innerTerm = zipWith (-) (replicate (I2 n cAll) (zipWith (+) alphas sumQs))
                                (map (* 0.5) $ sqsum (zipWith (+) eqxprod lxprod))
        slse = sum (logsumexp innerTerm)
    in zipWith (\a1 a2 -> -c1 + a1 + a2)
               (zipWith (-) slse
                            (map (fromIntegral n *) (logsumexp alphas)))
               (logWishartPrior k qdiags sumQs lvals wisGamma wisM prec)

newtype ObjectiveProgram = ObjectiveProgram (GMMIn -> Scalar FLT)
  deriving (Generic)
instance NFData ObjectiveProgram

run1Of :: (Arrays a, Arrays b) => BackendKind -> (Acc a -> Acc b) -> a -> b
run1Of Interpreter = I.run1
run1Of CPU = CPU.run1
-- run1Of GPU = GPU.run1

gmmObjectiveProgram :: BackendKind -> GMMIn -> ObjectiveProgram
gmmObjectiveProgram kind (precompute -> prec) =
    ObjectiveProgram (run1Of kind (\input -> objective input (constant prec)))

gmmObjective :: ObjectiveProgram -> GMMIn -> FLT
gmmObjective (ObjectiveProgram prog) input = prog input `linearIndexArray` 0

newtype ObjectiveGradProgram = ObjectiveGradProgram (GMMIn -> GMMOut)
  deriving (Generic)
instance NFData ObjectiveGradProgram

gmmObjectiveGradProgram :: BackendKind -> GMMIn -> ObjectiveGradProgram
gmmObjectiveGradProgram kind (precompute -> prec) =
    ObjectiveGradProgram $ run1Of kind $ \input ->
        let GMMIn a m i _ _ _ = gradientA (\input' -> objective input' (constant prec)) input
        in GMMOut a m i

gmmObjectiveGrad :: ObjectiveGradProgram -> GMMIn -> GMMOut
gmmObjectiveGrad (ObjectiveGradProgram prog) input = prog input
