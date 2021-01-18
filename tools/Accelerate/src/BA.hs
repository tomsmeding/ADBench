{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module BA (
    baObjective, baObjectiveProgram,
    baObjectiveJac, baObjectiveJacProgram,
) where

import Prelude ()
import Control.DeepSeq (NFData)
import Data.Array.Accelerate hiding (pi)
import qualified Data.Array.Accelerate.Interpreter as I
import qualified Data.Array.Accelerate.LLVM.Native as CPU
-- import qualified Data.Array.Accelerate.LLVM.PTX as GPU

import BAIO
import Types


camIndex :: Exp Camera -> Exp Int -> Exp FLT
camIndex (Camera (T3 r1 r2 r3) (T3 c1 c2 c3) f (T2 x1 x2) (T2 k1 k2)) =
    pick [r1, r2, r3, c1, c2, c3, f, x1, x2, k1, k2]
  where
    pick [x] _ = x
    pick (x:xs) i = cond (i == 0) x (pick xs (i-1))
    pick [] _ = error "pick: what"

ptIndex :: Exp Pt3D -> Exp Int -> Exp FLT
ptIndex (T3 x y z) i = cond (i == 0) x (cond (i == 1) y z)


vecadd :: TLift t FLT => Exp t -> Exp t -> Exp t
vecadd = tlift @_ @FLT (+)

vecsub :: TLift t FLT => Exp t -> Exp t -> Exp t
vecsub = tlift @_ @FLT (-)

dot :: TLift t FLT => Exp t -> Exp t -> Exp FLT
dot v w = treduce (+) (tlift @_ @FLT (*) v w)

scale :: TLift t FLT => Exp FLT -> Exp t -> Exp t
scale = tlift1 (*)

cross :: Exp Pt3D -> Exp Pt3D -> Exp Pt3D
cross (T3 a b c) (T3 x y z) = T3 (b*z - c*y) (c*x - a*z) (a*y - b*x)

rodriguez :: Exp Pt3D -> Exp Pt3D -> Exp Pt3D
rodriguez r@(T3 r1 r2 r3) x =
    let sqtheta = r1 * r1 + r2 * r2 + r3 * r3
        theta = sqrt sqtheta
        v = T3 (r1 / theta) (r2 / theta) (r3 / theta)
    in cond (sqtheta == 0)
            (x `vecadd` cross r x)
            (scale (cos theta) x
                `vecadd` scale (sin theta) (cross v x)
                `vecadd` scale ((v `dot` x) * (1 - cos theta)) v)

p2e :: Exp Pt3D -> Exp Pt2D
p2e (T3 x1 x2 x3) = T2 (x1 / x3) (x2 / x3)

distort :: Exp Pt2D -> Exp Pt2D -> Exp Pt2D
distort (T2 k1 k2) u@(T2 u1 u2) =
    let lensq = u1 * u1 + u2 * u2
    in scale (1 + k1 * lensq + k2 * lensq * lensq) u

project :: Exp Camera -> Exp Pt3D -> Exp Pt2D
project (Camera r c f x0 kappa) x =
    scale f (distort kappa (p2e (rodriguez r (x `vecsub` c)))) `vecadd` x0

reprojerr :: Exp Camera -> Exp Pt3D -> Exp FLT -> Exp Pt2D -> Exp Pt2D
reprojerr cam x w m = scale w (project cam x `vecsub` m)

werr :: Exp FLT -> Exp FLT
werr w = 1 - w * w

objective :: Acc BAIn -> Acc BAFVal
objective (BAIn cams pts ws feats obs) =
    BAFVal (map (\(T2 i (Observation ci pi)) ->
                     reprojerr (cams ! I1 ci) (pts ! I1 pi) (ws ! i) (feats ! i))
                (indexed obs))
           (map werr ws)

-- We compute the Jacobian chunks in the following packed arrays:
--   - 2p x 11: d(flatten(reproj_error)) / d(cams_{obs[i]})
--   - 2p x 3: d(flatten(reproj_error)) / d(pts_{obs[i]})
--   - 2p: d(flatten(reproj_error)) / d(w_i)
--   - p: d(w_error) / d(w_i)
jacobianPacked :: Acc BAIn -> Acc (Matrix FLT, Matrix FLT, Vector FLT, Vector FLT)
jacobianPacked (BAIn cams pts ws feats obs) =
    let I1 p = shape obs
        grads :: Acc (Vector ((Camera, Camera), (Pt3D, Pt3D), (FLT, FLT)))
        grads = map (\(T2 i (Observation ci pi)) ->
                        let T4 dcam1 dpt1 dw1 _ =
                                gradientE (\(T4 cam pt w feat) -> fst (reprojerr cam pt w feat))
                                          (T4 (cams ! I1 ci) (pts ! I1 pi) (ws ! i) (feats ! i))
                            T4 dcam2 dpt2 dw2 _ =
                                gradientE (\(T4 cam pt w feat) -> snd (reprojerr cam pt w feat))
                                          (T4 (cams ! I1 ci) (pts ! I1 pi) (ws ! i) (feats ! i))
                        in T3 (T2 dcam1 dcam2) (T2 dpt1 dpt2) (T2 dw1 dw2))
                    (indexed obs)
        campacked = generate (I2 (2 * p) 11)
                             (\(I2 i j) -> let T3 (T2 dcam1 dcam2) _ _ = grads ! I1 (i `div` 2)
                                           in camIndex (cond (i `mod` 2 == 0) dcam1 dcam2) j)
        ptpacked = generate (I2 (2 * p) 3)
                            (\(I2 i j) -> let T3 _ (T2 dpt1 dpt2) _ = grads ! I1 (i `div` 2)
                                          in ptIndex (cond (i `mod` 2 == 0) dpt1 dpt2) j)
        rwpacked = generate (I1 (2 * p))
                            (\(I1 i) -> let T3 _ _ (T2 dw1 dw2) = grads ! I1 (i `div` 2)
                                        in cond (i `mod` 2 == 0) dw1 dw2)
        wwpacked = map (gradientE werr) ws
    in T4 campacked ptpacked rwpacked wwpacked

jacobian :: Acc BAIn -> Acc BAOut
jacobian input@(BAIn cams pts _ _ obs) =
    let I1 n = shape cams
        I1 m = shape pts
        I1 p = shape obs

        T4 dcams dpts drdw dwdw = jacobianPacked input

        rows = enumFromStepN (I1 (2 * p)) 0 15 ++ enumFromN (I1 (p + 1)) (2 * p * 15)
        colsvals =
            generate (I1 (2 * p * 15))
                     (\(I1 i) -> let obsi = i `div` (2 * 15)
                                     valouti = i `div` 15
                                     Observation ci pi = obs ! I1 obsi
                                     j = i `mod` 15
                                 in cond (j < 11)
                                         (T2 (11 * ci + j) (dcams ! I2 valouti j))
                                         (cond (j < 14)
                                               (T2 (11 * n + 3 * pi + j - 11) (dpts ! I2 valouti (j - 11)))
                                               (T2 (11 * n + 3 * m + obsi) (drdw ! I1 valouti))))
            ++
            generate (I1 p)
                     (\(I1 i) -> T2 (11 * n + 3 * m + i) (dwdw ! I1 i))
    in BAOut rows (map fst colsvals) (map snd colsvals)


newtype ObjectiveProgram = ObjectiveProgram (BAIn -> BAFVal)
  deriving (Generic)
instance NFData ObjectiveProgram

run1Of :: (Arrays a, Arrays b) => BackendKind -> (Acc a -> Acc b) -> a -> b
run1Of Interpreter = I.run1
run1Of CPU = CPU.run1
-- run1Of GPU = GPU.run1

baObjectiveProgram :: BackendKind -> ObjectiveProgram
baObjectiveProgram kind = ObjectiveProgram (run1Of kind objective)

baObjective :: ObjectiveProgram -> BAIn -> BAFVal
baObjective (ObjectiveProgram prog) input = prog input

newtype ObjectiveJacProgram = ObjectiveJacProgram (BAIn -> BAOut)
  deriving (Generic)
instance NFData ObjectiveJacProgram

baObjectiveJacProgram :: BackendKind -> ObjectiveJacProgram
baObjectiveJacProgram kind = ObjectiveJacProgram $ run1Of kind jacobian

baObjectiveJac :: ObjectiveJacProgram -> BAIn -> BAOut
baObjectiveJac (ObjectiveJacProgram prog) input = prog input


class (Elt t, Elt a) => TLift t a where
    tlift :: (Exp a -> Exp a -> Exp a) -> Exp t -> Exp t -> Exp t
    tlift1 :: (Exp a -> Exp a -> Exp a) -> Exp a -> Exp t -> Exp t
    treduce :: (Exp a -> Exp a -> Exp a) -> Exp t -> Exp a

instance Elt a => TLift a a where
    tlift f = f
    tlift1 f = f
    treduce _ x = x

instance (TLift t1 a, TLift t2 a) => TLift (t1, t2) a where
    tlift f (T2 a1 a2) (T2 b1 b2) = T2 (tlift f a1 b1) (tlift f a2 b2)
    tlift1 f a (T2 b1 b2) = T2 (tlift1 f a b1) (tlift1 f a b2)
    treduce f (T2 x y) = treduce f x `f` treduce f y

instance (TLift t1 a, TLift t2 a, TLift t3 a) => TLift (t1, t2, t3) a where
    tlift f (T3 a1 a2 a3) (T3 b1 b2 b3) = T3 (tlift f a1 b1) (tlift f a2 b2) (tlift f a3 b3)
    tlift1 f a (T3 b1 b2 b3) = T3 (tlift1 f a b1) (tlift1 f a b2) (tlift1 f a b3)
    treduce f (T3 x y z) = treduce f x `f` treduce f y `f` treduce f z
