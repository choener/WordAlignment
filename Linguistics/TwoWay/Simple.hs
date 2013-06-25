{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

module Linguistics.TwoWay.Simple
  ( twoWay
  ) where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.ByteString.Char8 (ByteString)
import Data.Vector.Fusion.Util (Id(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as S
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import ADP.Fusion
import ADP.Fusion.Chr
import ADP.Fusion.Empty
import ADP.Fusion.Table
import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import Linguistics.Common
import Linguistics.TwoWay.Common



sScore :: Monad m => [Double] -> Double -> STwoWay m Double Double ByteString ()
sScore scores gapOpen = STwoWay
  { loop_step = \ww (Z:.():.c)     -> ww + gapOpen
  , step_loop = \ww (Z:.c:.())     -> ww + gapOpen
  , step_step = \ww (Z:.c:.d ) -> let cev = T.any vowel     $ T.decodeUtf8 c
                                      cec = T.any consonant $ T.decodeUtf8 c
                                      dev = T.any vowel     $ T.decodeUtf8 d
                                      dec = T.any consonant $ T.decodeUtf8 d
                                  in ww + if
                  | c==d && cec -> consonantIDS
                  | c==d && cev -> vowelIDS
                  | cev && dev  -> vowelS
                  | cec && dec  -> consonantS
                  | cev && dec || cec && dev -> vowelConsonantS
                  | otherwise   -> otherS
  , nil_nil   = const 0
  , h         = S.foldl' max (-500000)
  } where
    vowel     x = x `elem` "aeiou"
    consonant x = x >= 'a' && x <= 'z' && (not $ vowel x)
    [consonantIDS,consonantS,vowelIDS,vowelS,otherS,vowelConsonantS] = scores
{-# INLINE sScore #-}

sAlign :: Monad m => STwoWay m (String,String) (S.Stream m (String,String)) ByteString ()
sAlign = STwoWay
  { loop_step = \(w1,w2) (Z:.():.c) -> (w1++padd "" c, w2++prnt c "")
  , step_loop = \(w1,w2) (Z:.c:.()) -> (w1++prnt c "", w2++padd "" c)
  , step_step = \(w1,w2) (Z:.a:.b) -> (w1++prnt a b,w2++prnt b a)
  , nil_nil   = const ("","")
  , h         = return . id
  } where prnt x z = printAligned x [z]
          padd x z = printAlignedPad '-' x [z]
{-# INLINE sAlign #-}

twoWay scores gapOpen i1 i2 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2), bt) where
  ws = unsafePerformIO (twoWayFill scores gapOpen i1 i2)
  n1 = V.length i1
  n2 = V.length i2
  bt = backtrack scores gapOpen i1 i2 ws
{-# NOINLINE twoWay #-}

twoWayFill
  :: [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> IO (PA.Unboxed (Z:.PointL:.PointL) Double)
twoWayFill scores gapOpen i1 i2 = do
  let n1 = V.length i1
  let n2 = V.length i2
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2) 0
  let w = mTbl (Z:.EmptyT:.EmptyT) t'
  fillTable2 $ gTwoWay (sScore scores gapOpen) w (chr i1) (chr i2) Empty Empty
  freeze t'
{-# INLINE twoWayFill #-}

backtrack
  :: [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> PA.Unboxed (Z:.PointL:.PointL) Double
  -> [(String,String)]
backtrack scores gapOpen i1 i2 tbl = unId . S.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 where
  n1 = V.length i1
  n2 = V.length i2
  w :: DefBtTbl Id (Z:.PointL:.PointL) Double (String,String)
  w = btTbl (Z:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL) -> Id (S.Stream Id (String,String)))
  (Z:.(_,g)) = gTwoWay (sScore scores gapOpen <** sAlign) w (chr i1) (chr i2) Empty Empty
{-# INLINE backtrack #-}

test s = twoWay [3,1,1,0,0,-1] (-1) s' s' where
  s' = V.fromList $ L.map (B.pack . (:[])) $ s

