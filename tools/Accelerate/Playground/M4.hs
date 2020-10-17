{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS -Wno-name-shadowing -Wno-unused-local-binds -Wno-unused-matches #-}
module Playground.M4 (withoutExpTemps) where

import Prelude (id)
import Data.Array.Accelerate

import GMMIO
import Playground.Support


{-# NOINLINE withoutExpTemps #-}
withoutExpTemps :: Acc GMMIn -> Acc (Vector Float, Matrix Float, Matrix Float)
withoutExpTemps input =
    let
      GMMIn a0 a1 a2 a3 a4 a5 = input
      a6 :: Acc (Scalar (Float, (Int, Float)))
      a6f f = map (\x0 -> let x1 = fromIntegral x0 in f x1 (T2 x0 x1)) a5
      a6 = a6f t2
      shape_a7 :: Exp DIM2
      shape_a7 = I2 (let I1 x0 = shape a0 in x0) (let I2 x0 x1 = shape a3 in x1)
      a7 :: Acc (Matrix Float)
      a7 = backpermute shape_a7 id a2
      a8 :: Acc (Vector Float)
      a8 = fold (+) 0.0 a7
      a9f f = zipWith
             (\x0 x1 -> let x2 = x0 * x1 in f x2 (T3 x0 x1 x2))
             a8
             (replicate (shape a0) (map fst a6))
      a9 = a9f t2
      a10 = map (\x0 -> let x1 = 0.5 * x0
                            x2 = x1 * x0
                        in T2 x2 (T4 (0.5 :: Exp Float) x0 x1 x2)) a4
      a11f f = map (\x0 -> let x1 = x0 * x0 in f x1 (T2 x0 x1))
                (backpermute
                   (I2 (let I1 x0 = shape a0 in x0)
                       (let I2 x0 x1 = shape a2 in x1 - let I2 x0 x1 = shape a3 in x1))
                   (\(I2 x0 x1) -> I2 x0 (x1 + let I2 x2 x3 = shape a3 in x3)) a2)
      a11 = a11f t2
      shape_a12 :: Exp DIM2
      shape_a12 = I2 (let I1 x0 = shape a0 in x0) ((let I2 x0 x1 = shape a2 in x1) - let I2 x0 x1 = shape a3 in x1)
      a12 = map fst a11
      a13f f = map (\x0 -> let x1 = exp x0 in f x1 (T2 x0 x1)) a7
      a13 = a13f t2
      a14 = map fst a13
      a15f f = map (\x0 -> let x1 = x0 * x0 in f x1 (T2 x0 x1)) a14
      a15 = a15f t2
      shape_a16 :: Exp DIM2
      shape_a16 = shape_a7
      a16 = map fst a15
      a17f f = zipWith
              (\x0 x1 -> let x2 = x0 + x1 in f x2 (T3 x0 x1 x2))
              (fold (+) 0.0 a16)
              (fold (+) 0.0 a12)
      a17 = a17f t2
      a18f f = zipWith
              (\x0 x1 -> let x2 = x0 * x1 in f x2 (T3 x0 x1 x2))
              (map fst a17)
              (replicate (shape a0) (map fst a10))
      a18 = a18f t2
      a19f f = zipWith
              (\x0 x1 -> let x2 = x0 - x1 in f x2 (T3 x0 x1 x2))
              (map fst a18)
              (map fst a9)
      a19 = a19f t2
      shape_a20 :: Exp DIM1
      shape_a20 = shmin (shmin (shape a0) (indexTail shape_a7)) (indexTail shape_a12)
      a20 = map fst a19
      a21f f = map (\x0 -> let x1 = x0 - (-12.655121) in f x1 (T3 x0 (-12.655121 :: Exp Float) x1))
                (fold (+) 0.0 a20)
      a21 = a21f t2
      a22 = fold1 max a0
      a23f f = zipWith
              (\x0 x1 -> let x2 = x0 - x1 in f x2 (T3 x0 x1 x2))
              a0
              (replicate (shape a0) a22)
      a23 = a23f t2
      shape_a24 :: Exp DIM1
      shape_a24 = shape a0
      a24f f = map (\x0 -> let x1 = exp x0 in f x1 (T2 x0 x1)) (map fst a23)
      a24 = a24f t2
      a25 = map fst a24
      a26f f = map (\x0 -> let x1 = log x0 in f x1 (T2 x0 x1)) (fold (+) 0.0 a25)
      a26 = a26f t2
      a27f f = zipWith (\x0 x1 -> let x2 = x0 + x1 in f x2 (T3 x0 x1 x2)) (map fst a26) a22
      a27 = a27f t2
      a28f f = map (\x0 -> let I2 x1 x2 = shape a3
                               x3 = fromIntegral x1
                               x4 = x3 * x0
                           in f x4 (T5 x1 x2 x3 x0 x4))
                (map fst a27)
      a28 = a28f t2
      shape_a29 :: Exp DIM3
      shape_a29 = shmin (let I2 y0 y1 = shape a3 in I3 y0 (let I1 x0 = shape a0 in x0) y1)
                        (let I2 y0 y1 = shape a1 in I3 (let I2 x0 x1 = shape a3 in x0) y0 y1)
      a29f f = zipWith
              (\x0 x1 -> let x2 = x0 - x1 in f x2 (T3 x0 x1 x2))
              (replicate (I3 cAll (let I1 x0 = shape a0 in x0) cAll) a3)
              (replicate (I3 (let I2 x0 x1 = shape a3 in x0) cAll cAll) a1)
      a29 = a29f t2
      a30 = map fst a29
      a31 = generate
              (I2 (let I1 x0 = shape a0 in x0)
               (let I2 x0 x1 = shape a2 in x1 - (let I2 x0 x1 = shape a3 in x1) + 1))
              (\(I2 x0 x1) -> let x2 = x1 == 0
                                  x3 = if x2 then 0.0 else 1.0
                              in T2 x3 (T7 x0 x1 (0 :: Exp Int) x2 (0.0 :: Exp Float) (1.0 :: Exp Float) x3))
      a32f f = zipWith
              (\x0 x1 -> let x2 = x0 * x1 in f x2 (T3 x0 x1 x2))
              (map fst a31)
              (backpermute
                 (I2 (let I1 x0 = shape a0 in x0)
                  (let I2 x0 x1 = shape a2 in x1 - (let I2 x0 x1 = shape a3 in x1) + 1))
                 (\(I2 x0 x1) -> I2 x0 (x1 + let I2 x2 x3 = shape a3 in x3 - 1))
                 a2)
      a32 = a32f t2
      shape_a33 :: Exp DIM2
      shape_a33 = I2 (let I1 x0 = shape a0 in x0) (let I2 x0 x1 = shape a2 in x1 - (let I2 x0 x1 = shape a3 in x1) + 1)
      a33 = map fst a32
      a34f f = zipWith
              (\x0 x1 -> let x2 = x0 * x1 in f x2 (T3 x0 x1 x2))
              (backpermute
                 (let I2 x0 x1 = shape a3 in I4 x0 (let I1 x2 = shape a0 in x2) x1 x1)
                 (\(I4 x0 x1 x2 x3) ->
                    if x2 > x3
                       then
                         I2 x1
                         (let I2 x4 x5 = shape a3 in div (x5 * (x5 - 1)) 2 - div ((x5 - 1 - x3) * (x5 - 1 - x3 + 1)) 2
                          + x2
                          - x3
                          - 1
                          + 1)
                       else I2 x1 0)
                 a33)
              (replicate (I4 cAll cAll (let I2 x0 x1 = shape a3 in x1) cAll) a30)
      a34 = a34f t2
      shape_a35 :: Exp DIM4
      shape_a35 = shmin (let I2 x0 x1 = shape a3 in I4 x0 (let I1 x2 = shape a0 in x2) x1 x1)
                        (let I3 y0 y1 y2 = shape_a29 in I4 y0 y1 (let I2 x0 x1 = shape a3 in x1) y2)
      a35 = map fst a34
      a36f f = zipWith
              (\x0 x1 -> let x2 = x0 * x1 in f x2 (T3 x0 x1 x2))
              (replicate (I3 (let I2 x0 x1 = shape a3 in x0) cAll cAll) a14)
              a30
      a36 = a36f t2
      a37f f = zipWith
              (\x0 x1 -> let x2 = x0 + x1 in f x2 (T3 x0 x1 x2))
              (map fst a36)
              (fold (+) 0.0 a35)
      a37 = a37f t2
      a38f f = map (\x0 -> let x1 = x0 * x0 in f x1 (T2 x0 x1)) (map fst a37)
      a38 = a38f t2
      shape_a39 :: Exp DIM3
      shape_a39 = shmin (shmin (let I2 y0 y1 = shape_a7 in I3 (let I2 x0 x1 = shape a3 in x0) y0 y1) shape_a29)
                        (indexTail shape_a35)
      a39 = map fst a38
      a40f f = map (\x0 -> let x1 = x0 * 0.5 in f x1 (T3 x0 (0.5 :: Exp Float) x1)) (fold (+) 0.0 a39)
      a40 = a40f t2
      a41f f = zipWith (\x0 x1 -> let x2 = x0 + x1 in f x2 (T3 x0 x1 x2)) a0 a8
      a41 = a41f t2
      a42f f = zipWith
              (\x0 x1 -> let x2 = x0 - x1 in f x2 (T3 x0 x1 x2))
              (replicate (I2 (let I2 x0 x1 = shape a3 in x0) cAll) (map fst a41))
              (map fst a40)
      a42 = a42f t2
      shape_a43 :: Exp DIM2
      shape_a43 = shmin (let I1 y0 = shmin (shape a0) (indexTail shape_a7) in I2 (let I2 x0 x1 = shape a3 in x0) y0) (indexTail shape_a39)
      a43 = map fst a42
      a44 = fold1 max a43
      a45f f = zipWith
              (\x0 x1 -> let x2 = x0 - x1 in f x2 (T3 x0 x1 x2))
              a43
              (replicate (I2 cAll (let I2 x0 x1 = shape_a43 in x1)) a44)
      a45 = a45f t2
      a46f f = map (\x0 -> let x1 = exp x0 in f x1 (T2 x0 x1)) (map fst a45)
      a46 = a46f t2
      shape_a47 :: Exp DIM2
      shape_a47 = shmin shape_a43 (let I1 y0 = indexTail shape_a43 in I2 y0 (let I2 x0 x1 = shape_a43 in x1))
      a47 = map fst a46
      a48f f = map (\x0 -> let x1 = log x0 in f x1 (T2 x0 x1)) (fold (+) 0.0 a47)
      a48 = a48f t2
      a49f f = zipWith (\x0 x1 -> let x2 = x0 + x1 in f x2 (T3 x0 x1 x2)) (map fst a48) a44
      a49 = a49f t2
      shape_a50 :: Exp DIM1
      shape_a50 = shmin (indexTail shape_a47) (indexTail shape_a43)
      a50 = map fst a49
      a51f f = zipWith
              (\x0 x1 -> let x2 = x0 - x1 in f x2 (T3 x0 x1 x2))
              (fold (+) 0.0 a50)
              (map fst a28)
      a51 = a51f t2
      a52f f = zipWith
              (\x0 x1 -> let x2 = -1837.8771
                             x3 = x2 + x0
                             x4 = x3 + x1
                         in f x4 (T6 (1837.8771 :: Exp Float) x2 x0 x1 x3 x4))
              (map fst a51)
              (map fst a21)
      a52 = a52f t2
      a53 = zipWith
              (\x0 (T6 x1 x2 x3 x4 x5 x6) -> T2 (0.0 + x0) (x0 + 0.0))
              (generate Z_ (\Z_ -> 1.0))
              (a52f const')
      a54 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (-x0 + 0.0))
              (map fst a53)
              (a51f const')
      a55 = map fst a54
      a56 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (x0 + 0.0))
              (generate shape_a50 (\(I1 _) -> a55 ! Z_))
              (a49f const')
      a57 = zipWith
              (\x0 (T2 x1 x2) -> x0 / x1)
              (map fst a56)
              (a48f const')
      a58 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (-x0 + 0.0))
              (zipWith
                 (\x0 (T2 x1 x2) -> x0 * x2)
                 (generate shape_a47 (\(I2 x0 _) -> a57 ! I1 x0))
                 (a46f const'))
              (a45f const')
      a59 = scanl1 max a43
      a60 = backpermute (let I2 x0 x1 = shape a59 in I2 x0 (x1 - 1)) id a59
      a61 = zipWith (*)
              (let
                 a61 = zipWith
                         (\x0 x1 ->
                            let T2 _ x2 = let T2 x2 x3 = if x0 > x1 then T2 1.0 0.0 else T2 0.0 1.0
                                          in T2 (0.0 + x2 :: Exp Float) (x3 + 0.0)
                            in x2)
                         a60
                         (backpermute (let I2 x0 x1 = shape_a43 in I2 x0 (x1 - 1)) (\(I2 x0 x1) -> I2 x0 (x1 + 1)) a43)
               in
               generate
                 (let I2 x0 x1 = shape a61 in I2 x0 (x1 + 1))
                 (\(I2 x0 x1) -> if x1 > 0 then a61 ! (I2 x0 (x1 - 1)) else 1.0))
              (scanr (*)
                 1.0
                 (zipWith
                    (\x0 x1 ->
                       let T2 x2 _ = let T2 x2 x3 = if x0 > x1 then T2 1.0 0.0 else T2 0.0 1.0
                                     in T2 (0.0 + x2) (x3 + 0.0 :: Exp Float)
                       in x2)
                    a60
                    (backpermute (let I2 x0 x1 = shape_a43 in I2 x0 (x1 - 1)) (\(I2 x0 x1) -> I2 x0 (x1 + 1)) a43)))
      a62 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (-x0 + 0.0))
              (zipWith (+)
                 (zipWith (*)
                    (replicate
                       (let I2 _ x0 = shape a61 in I2 cAll x0)
                       (zipWith (+)
                          (let a62 = map snd a58
                           in
                           fold1 (+)
                             (reshape
                                (let I2 x0 x1 = let I2 x0 x1 = shape a62 in I2 x0 x1 in I2 x0 x1)
                                (backpermute (shape a62) id a62)))
                          (map snd a56)))
                    a61)
                 (map fst a58))
              (a42f const')
      a63 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (x0 + 0.0))
              (let a63 = map fst a62
               in
               fold1 (+)
                 (reshape
                    (let I2 x0 x1 = let I2 x0 x1 = shape a63 in I2 x1 x0 in I2 x0 x1)
                    (backpermute (let I2 x0 x1 = shape a63 in I2 x1 x0) (\(I2 x0 x1) -> I2 x1 x0) a63)))
              (a41f const')
      a64 = zipWith
              (\x0 (T3 x1 x2 x3) -> x0 * x2)
              (map snd a62)
              (a40f const')
      a65 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (x0 + 0.0))
              (zipWith
                 (\x0 (T2 x1 x2) -> x0 * x1 + x0 * x1)
                 (generate shape_a39 (\(I3 x0 x1 _) -> a64 ! (I2 x0 x1)))
                 (a38f const'))
              (a37f const')
      a66 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0 * x2) (x0 * x1 + 0.0))
              (map fst a65)
              (a36f const')
      a67 = map snd a65
      a68 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0 * x2) (x0 * x1 + 0.0))
              (generate shape_a35 (\(I4 x0 x1 x2 _) -> a67 ! (I3 x0 x1 x2)))
              (a34f const')
      a69 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (-x0 + 0.0))
              (zipWith (+)
                 (let a69 = map snd a68
                  in
                  fold1 (+)
                    (reshape
                       (let I4 x0 x1 x2 x3 = let I4 x0 x1 x2 x3 = shape a69 in I4 x0 x1 x3 x2 in I4 x0 x1 x2 x3)
                       (backpermute
                          (let I4 x0 x1 x2 x3 = shape a69 in I4 x0 x1 x3 x2)
                          (\(I4 x0 x1 x2 x3) -> I4 x0 x1 x3 x2)
                          a69)))
                 (map snd a66))
              (a29f const')
      a70 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (x0 + 0.0))
              (zipWith
                 (\x0 (T5 x1 x2 x3 x4 x5) -> x0 * x3)
                 (map snd a54)
                 (a28f const'))
              (a27f const')
      a71 = zipWith
              (\x0 (T2 x1 x2) -> x0 / x1)
              (map fst a70)
              (a26f const')
      a72 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (-x0 + 0.0))
              (zipWith
                 (\x0 (T2 x1 x2) -> x0 * x2)
                 (generate shape_a24 (\(I1 _) -> a71 ! Z_))
                 (a24f const'))
              (a23f const')
      a73 = scanl1 max a0
      a74 = backpermute (let I1 x0 = shape a73 in I1 (x0 - 1)) id a73
      a75 = zipWith
              (*)
              (let
                 a75 = zipWith
                         (\x0 x1 ->
                            let T2 _ x2 = let T2 x2 x3 = if x0 > x1 then T2 1.0 0.0 else T2 0.0 1.0
                                          in T2 (0.0 + x2 :: Exp Float) (x3 + 0.0)
                            in x2)
                         a74
                         (backpermute (let I1 x0 = shape a0 in I1 (x0 - 1)) (\(I1 x0) -> I1 (x0 + 1)) a0)
               in
               generate (let I1 x0 = shape a75 in I1 (x0 + 1)) (\(I1 x0) -> if x0 > 0 then a75 ! I1 (x0 - 1) else 1.0))
              (scanr
                 (*)
                 1.0
                 (zipWith
                    (\x0 x1 ->
                       let T2 x2 _ = let T2 x2 x3 = if x0 > x1 then T2 1.0 0.0 else T2 0.0 1.0
                                     in T2 (0.0 + x2) (x3 + 0.0 :: Exp Float)
                       in x2)
                    a74
                    (backpermute (let I1 x0 = shape a0 in I1 (x0 - 1)) (\(I1 x0) -> I1 (x0 + 1)) a0)))
      a76 = zipWith
              (\x0 (T3 x1 x2 x3) -> x0)
              (map snd a53)
              (a21f const')
      a77 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (-x0 + 0.0))
              (generate shape_a20 (\(I1 _) -> a76 ! Z_))
              (a19f const')
      a78 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0 * x2) (x0 * x1 + 0.0))
              (map fst a77)
              (a18f const')
      a79 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0) (x0 + 0.0))
              (map fst a78)
              (a17f const')
      a80 = map fst a79
      a81 = map snd a79
      a82 = zipWith
              (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0 * x2) (x0 * x1 + 0.0))
              (map snd a77)
              (a9f const')
      a83 = zipWith (+) (map fst a82) (map snd a63)
    in
    T3 (zipWith (+)
          (zipWith (+)
             (zipWith (+)
                (zipWith (+)
                   (zipWith (+)
                      (generate (shape a0) (\_ -> 0.0))
                      (generate (shape a0) (\_ -> 0.0)))
                   (generate (shape a0) (\_ -> 0.0)))
                (generate (shape a0) (\_ -> 0.0)))
             (generate (shape a0) (\_ -> 0.0)))
          (zipWith (+)
             (zipWith (+)
                (zipWith (+)
                   (zipWith (+)
                      (generate (shape a0) (\_ -> 0.0))
                      (generate (shape a0) (\_ -> 0.0)))
                   (generate (shape a0) (\_ -> 0.0)))
                (zipWith (+)
                   (zipWith (+)
                      (zipWith (+)
                         (generate (shape a0) (\_ -> 0.0))
                         (generate (shape a0) (\_ -> 0.0)))
                      (zipWith (+)
                         (zipWith (+)
                            (generate (shape a0) (\_ -> 0.0))
                            (zipWith (+)
                               (zipWith (+)
                                  (zipWith (+)
                                     (zipWith
                                        (*)
                                        (replicate
                                           (shape a75)
                                           (zipWith (+)
                                              (let a84 = map snd a72
                                               in
                                               fold1 (+)
                                                 (reshape
                                                    (let I1 x0 = let I1 x0 = shape a84 in I1 x0 in I1 x0)
                                                    (backpermute (shape a84) id a84)))
                                              (map snd a70)))
                                        a75)
                                     (map fst a72))
                                  (map fst a63))
                               (generate (shape a0) (\_ -> 0.0))))
                         (generate (shape a0) (\_ -> 0.0))))
                   (generate (shape a0) (\_ -> 0.0))))
             (generate (shape a0) (\_ -> 0.0))))
    (zipWith (+)
       (zipWith (+)
          (zipWith (+)
             (zipWith (+)
                (zipWith (+)
                   (generate (shape a1) (\_ -> 0.0))
                   (generate (shape a1) (\_ -> 0.0)))
                (generate (shape a1) (\_ -> 0.0)))
             (generate (shape a1) (\_ -> 0.0)))
          (zipWith (+)
             (zipWith (+)
                (zipWith (+)
                   (zipWith (+)
                      (generate (shape a1) (\_ -> 0.0))
                      (generate (shape a1) (\_ -> 0.0)))
                   (zipWith (+)
                      (zipWith (+)
                         (zipWith (+)
                            (generate (shape a1) (\_ -> 0.0))
                            (zipWith (+)
                               (zipWith (+)
                                  (zipWith (+)
                                     (generate (shape a1) (\_ -> 0.0))
                                     (let a84 = map snd a69
                                      in
                                      fold1 (+)
                                        (reshape
                                           (let I3 x0 x1 x2 = shape a84 in I3 x1 x2 x0)
                                           (backpermute
                                              (let I3 x0 x1 x2 = shape a84 in I3 x1 x2 x0)
                                              (\(I3 x0 x1 x2) -> I3 x2 x0 x1)
                                              a84))))
                                  (generate (shape a1) (\_ -> 0.0)))
                               (generate (shape a1) (\_ -> 0.0))))
                         (generate (shape a1) (\_ -> 0.0)))
                      (generate (shape a1) (\_ -> 0.0))))
                (generate (shape a1) (\_ -> 0.0)))
             (generate (shape a1) (\_ -> 0.0))))
       (generate (shape a1) (\_ -> 0.0)))
    (zipWith (+)
       (zipWith (+)
          (zipWith (+)
             (zipWith (+)
                (zipWith (+)
                   (generate (shape a2) (\_ -> 0.0))
                   (generate (shape a2) (\_ -> 0.0)))
                (generate (shape a2) (\_ -> 0.0)))
             (zipWith (+)
                (zipWith (+)
                   (zipWith (+)
                      (zipWith (+)
                         (generate (shape a2) (\_ -> 0.0))
                         (zipWith (+)
                            (zipWith (+)
                               (zipWith (+)
                                  (zipWith (+)
                                     (zipWith (+)
                                        (generate (shape a2) (\_ -> 0.0))
                                        (generate (shape a2) (\_ -> 0.0)))
                                     (zipWith (+)
                                        (zipWith (+)
                                           (permute
                                              (+)
                                              (generate (shape a2) (\_ -> 0.0))
                                              (\(I2 x0 x1) -> Just_ (I2 x0 x1))
                                              (zipWith (+)
                                                 (generate shape_a7 (\(I2 x0 _) -> a83 ! I1 x0))
                                                 (zipWith
                                                    (\x0 (T2 x1 x2) -> x0 * x2)
                                                    (zipWith (+)
                                                       (zipWith
                                                          (\x0 (T2 x1 _) -> x0 * x1 + x0 * x1)
                                                          (generate shape_a16 (\(I2 x0 _) -> a80 ! I1 x0))
                                                          (a15f const'))
                                                       (let a84 = map fst a66
                                                        in
                                                        fold1
                                                          (+)
                                                          (reshape
                                                             (let I3 x0 x1 x2 = shape a84 in I3 x1 x2 x0)
                                                             (backpermute
                                                                (let I3 x0 x1 x2 = shape a84 in I3 x1 x2 x0)
                                                                (\(I3 x0 x1 x2) -> I3 x2 x0 x1)
                                                                a84))))
                                                    (a13f const'))))
                                           (permute
                                              (+)
                                              (generate (shape a2) (\_ -> 0.0))
                                              (\(I2 x0 x1) -> Just_ (I2 x0 (x1 + let I2 x2 x3 = shape a3 in x3)))
                                              (zipWith
                                                 (\x0 (T2 x1 x2) -> x0 * x1 + x0 * x1)
                                                 (generate shape_a12 (\(I2 x0 _) -> a81 ! I1 x0))
                                                 (a11f const'))))
                                        (permute
                                           (+)
                                           (generate (shape a2) (\_ -> 0.0))
                                           (\(I2 x0 x1) -> Just_ (I2 x0 (x1 + let I2 x2 x3 = shape a3 in x3 - 1)))
                                           (map snd
                                              (zipWith
                                                 (\x0 (T3 x1 x2 x3) -> T2 (0.0 + x0 * x2) (x0 * x1 + 0.0))
                                                 (permute
                                                    (+)
                                                    (generate shape_a33 (\_ -> 0.0))
                                                    (\(I4 x0 x1 x2 x3) ->
                                                       let
                                                         T2 x4 x5 = if x2 > x3
                                                                       then
                                                                         T2 x1
                                                                         (let I2 x4 x5 = shape a3
                                                                          in
                                                                          div (x5 * (x5 - 1)) 2
                                                                          - div ((x5 - 1 - x3) * (x5 - 1 - x3 + 1)) 2
                                                                          + x2
                                                                          - x3
                                                                          - 1
                                                                          + 1)
                                                                       else T2 x1 0
                                                       in
                                                       Just_ (I2 x4 x5))
                                                    (map fst a68))
                                                 (a32f const'))))))
                                  (generate (shape a2) (\_ -> 0.0)))
                               (generate (shape a2) (\_ -> 0.0)))
                            (generate (shape a2) (\_ -> 0.0))))
                      (generate (shape a2) (\_ -> 0.0)))
                   (generate (shape a2) (\_ -> 0.0)))
                (generate (shape a2) (\_ -> 0.0))))
          (generate (shape a2) (\_ -> 0.0)))
       (generate (shape a2) (\_ -> 0.0)))

t2 :: (Elt a, Elt b) => Exp a -> Exp b -> Exp (a, b)
t2 x y = T2 x y

const' :: a -> b -> b
const' _ x = x
