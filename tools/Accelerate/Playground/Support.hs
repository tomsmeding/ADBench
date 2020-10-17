{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Playground.Support (
    cAll,
    ShapeMin(..)
) where

import Data.Array.Accelerate

import GMMIO


cAll :: Exp All
cAll = constant All

class Shape sh => ShapeMin sh where
    shmin :: Exp sh -> Exp sh -> Exp sh

instance ShapeMin Z where
    shmin Z_ Z_ = Z_

instance ShapeMin sh => ShapeMin (sh :. Int) where
    shmin (sh ::. i) (sh' ::. j) = shmin sh sh' ::. min i j
