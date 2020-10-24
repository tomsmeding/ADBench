{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module BAIO (
    Camera(..), pattern Camera,
    Observation(..), pattern Observation,
    BAIn(..), pattern BAIn,
    BAFVal(..), pattern BAFVal,
    BAOut(..), pattern BAOut,
    Pt3D, Pt2D,
    baReadInstance,
    baWriteFunctionOutput,
    baWriteJacobian
) where

import Control.DeepSeq (NFData)
import Data.Array.Accelerate (Elt, Arrays, Exp, Acc, Vector
                             ,Z(..), (:.)(..), pattern Pattern, Generic)
import qualified Data.Array.Accelerate as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Double.Conversion.ByteString (toFixed)
import System.IO (hPutStrLn, withFile, IOMode(WriteMode))

import Types


type Pt3D = (FLT, FLT, FLT)
type Pt2D = (FLT, FLT)

data Camera =
    Camera_ { camR     :: !Pt3D
            , camC     :: !Pt3D
            , camF     :: !FLT
            , camX0    :: !Pt2D
            , camKappa :: !Pt2D }
  deriving (Generic)

deriving instance Show Camera
instance Elt Camera
instance NFData Camera

pattern Camera :: Exp Pt3D -> Exp Pt3D -> Exp FLT -> Exp Pt2D -> Exp Pt2D -> Exp Camera
pattern Camera r c f x0 k = Pattern (r, c, f, x0, k)
{-# COMPLETE Camera #-}

data Observation =
    Observation_ { obsCamIdx :: !Int
                 , obsPtIdx  :: !Int }
  deriving (Generic)

deriving instance Show Observation
instance Elt Observation
instance NFData Observation

pattern Observation :: Exp Int -> Exp Int -> Exp Observation
pattern Observation ci pi = Pattern (ci, pi)
{-# COMPLETE Observation #-}

data BAIn =
    BAIn_ { baInCams  :: !(Vector Camera)        -- [n]
          , baInPts   :: !(Vector Pt3D)          -- [m]
          , baInW     :: !(Vector FLT)           -- [p]
          , baInFeats :: !(Vector Pt2D)          -- [p]
          , baInObs   :: !(Vector Observation) } -- [p]
  deriving (Generic)

deriving instance Show BAIn
instance Arrays BAIn
instance NFData BAIn

pattern BAIn :: Acc (Vector Camera) -> Acc (Vector Pt3D) -> Acc (Vector FLT) -> Acc (Vector Pt2D) -> Acc (Vector Observation) -> Acc BAIn
pattern BAIn c p w f o = Pattern (c, p, w, f, o)
{-# COMPLETE BAIn #-}

data BAFVal =
    BAFVal_ { baFValReprojErr :: !(Vector Pt2D)  -- [p]
            , baFValWErr      :: !(Vector FLT) } -- [p]
  deriving (Generic)

deriving instance Show BAFVal
instance Arrays BAFVal
instance NFData BAFVal

pattern BAFVal :: Acc (Vector Pt2D) -> Acc (Vector FLT) -> Acc BAFVal
pattern BAFVal r w = Pattern (r, w)
{-# COMPLETE BAFVal #-}

data BAOut =
    BAOut_ { baOutRows :: !(Vector Int)   -- [2p + p]
           , baOutCols :: !(Vector Int)   -- [11n + 3m + p]
           , baOutVals :: !(Vector FLT) } -- [31p]
  deriving (Generic)

deriving instance Show BAOut
instance Arrays BAOut
instance NFData BAOut

pattern BAOut :: Acc (Vector Int) -> Acc (Vector Int) -> Acc (Vector FLT) -> Acc BAOut
pattern BAOut r c v = Pattern (r, c, v)
{-# COMPLETE BAOut #-}

baReadInstance :: FilePath -> IO BAIn
baReadInstance fpath = do
    terms <- words <$> readFile fpath
    let slice i cnt = take cnt (drop i terms)
        [read -> n, read -> m, read -> p] = slice 0 3
        [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11] = map read (slice 3 11)
        cam = Camera_ (c1, c2, c3) (c4, c5, c6) c7 (c8, c9) (c10, c11)
        [p1, p2, p3] = map read (slice 14 3)
        pt = (p1, p2, p3)
        w = read (terms !! 17)
        [f1, f2] = map read (slice 18 2)
        feat = (f1, f2)
        reparr :: Elt a => Int -> a -> Vector a
        reparr cnt x = A.fromFunction (Z :. cnt) (const x)
    return $ BAIn_ { baInCams  = reparr n cam
                   , baInPts   = reparr m pt
                   , baInW     = reparr p w
                   , baInFeats = reparr p feat
                   , baInObs   = A.fromFunction (Z :. p) $
                                    \(Z :. i) -> Observation_ (i `rem` n) (i `rem` m)
                   }

baWriteFunctionOutput :: FilePath -> BAFVal -> IO ()
baWriteFunctionOutput fpath fval =
    let reprojErrs = concatMap (\(x,y) -> [x,y]) (A.toList (baFValReprojErr fval))
        wErrs = A.toList (baFValWErr fval)
    in withFile fpath WriteMode $ \f -> do
        hPutStrLn f "Reprojection error:"
        BS.hPut f $ BS.intercalate (BS.pack [10]) (map (toFixed 15 . realToFrac) reprojErrs)
        hPutStrLn f ""

        hPutStrLn f "Zach weight error:"
        BS.hPut f $ BS.intercalate (BS.pack [10]) (map (toFixed 15 . realToFrac) wErrs)
        hPutStrLn f ""

baWriteJacobian :: FilePath -> BAIn -> BAOut -> IO ()
baWriteJacobian fpath input out =
    let n = A.arraySize (baInCams input)
        m = A.arraySize (baInPts input)
        p = A.arraySize (baInW input)
        nrows = 2 * p + p
        ncols = 11 * n + 3 * m + p
        rows = A.toList (baOutRows out)
        cols = A.toList (baOutCols out)
        vals = A.toList (baOutVals out)
    in withFile fpath WriteMode $ \f -> do
        hPutStrLn f $ unwords [show nrows, show ncols]

        hPutStrLn f $ show (A.arraySize (baOutRows out))
        BS.hPut f $ BS.intercalate (BS.pack [32]) (map (toByteString . BSB.intDec) rows)
        hPutStrLn f ""

        hPutStrLn f $ show (A.arraySize (baOutCols out))
        BS.hPut f $ BS.intercalate (BS.pack [32]) (map (toByteString . BSB.intDec) cols)
        hPutStrLn f ""

        BS.hPut f $ BS.intercalate (BS.pack [32]) (map (toFixed 15 . realToFrac) vals)
        hPutStrLn f ""
  where
    toByteString = BSL.toStrict . BSB.toLazyByteString
