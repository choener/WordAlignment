{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

module Linguistics.FourWay.Simple
  ( fourWay
  ) where

import Data.Array.Repa.Index
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.ByteString.Char8 (ByteString)
import qualified Data.List as L
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.ByteString.Char8 (ByteString)
import Data.Vector.Fusion.Util (Id(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Unboxed as VU
import System.IO.Unsafe (unsafePerformIO)

import ADP.Fusion
import ADP.Fusion.Chr
import ADP.Fusion.Empty
import ADP.Fusion.Table
import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import Linguistics.Scoring.Simple
import Linguistics.FourWay.Common



sScore :: Monad m => VU.Vector Char -> VU.Vector Char -> [Double] -> Double -> SFourWay m Double Double ByteString ()
sScore !vowels !consonants !scores !gapOpen = SFourWay
  { loop_loop_loop_step = \ww (Z:.():.():.():.d ) -> ww + 3 * gapOpen + 3 * 0 -- each function has 6 elements + ww
  , loop_loop_step_loop = \ww (Z:.():.():.c :.()) -> ww + 3 * gapOpen + 3 * 0
  , loop_loop_step_step = \ww (Z:.():.():.c :.d ) -> ww + 4 * gapOpen + 1 * 0 + sm c d
  , loop_step_loop_loop = \ww (Z:.():.b :.():.()) -> ww + 3 * gapOpen + 3 * 0
  , loop_step_loop_step = \ww (Z:.():.b :.():.d ) -> ww + 4 * gapOpen + 1 * 0 + sm b d
  , loop_step_step_loop = \ww (Z:.():.b :.c :.()) -> ww + 4 * gapOpen + 1 * 0 + sm b c
  , loop_step_step_step = \ww (Z:.():.b :.c :.d ) -> ww + 3 * gapOpen + 0 * 0 + sm b c + sm b d + sm c d
  , step_loop_loop_loop = \ww (Z:.a :.():.():.()) -> ww + 3 * gapOpen + 3 * 0
  , step_loop_loop_step = \ww (Z:.a :.():.():.d ) -> ww + 4 * gapOpen + 1 * 0 + sm a d
  , step_loop_step_loop = \ww (Z:.a :.():.c :.()) -> ww + 4 * gapOpen + 1 * 0 + sm a c
  , step_loop_step_step = \ww (Z:.a :.():.c :.d ) -> ww + 3 * gapOpen + 0 * 0 + sm a c + sm a d + sm c d
  , step_step_loop_loop = \ww (Z:.a :.b :.():.()) -> ww + 4 * gapOpen + 1 * 0 + sm a b
  , step_step_loop_step = \ww (Z:.a :.b :.():.d ) -> ww + 3 * gapOpen + 0 * 0 + sm a b + sm a d + sm b d
  , step_step_step_loop = \ww (Z:.a :.b :.c :.()) -> ww + 3 * gapOpen + 0 * 0 + sm a b + sm a c + sm b c
  , step_step_step_step = \ww (Z:.a :.b :.c :.d ) -> ww + 0 * gapOpen + 0 * 0 + sm a b + sm a c + sm a d + sm b c + sm b d + sm c d
  , nil_nil_nil_nil = const 0
  , h         = S.foldl' max (-500000)
  } where sm = scoreMatch vowels consonants scores gapOpen
          {-# INLINE sm #-}
{-# INLINE sScore #-}

sAlign :: Monad m => SFourWay m Aligned (S.Stream m Aligned) ByteString ()
sAlign = SFourWay
  { loop_loop_loop_step = \(w1,w2,w3,w4) (Z:.():.():.():.d ) -> (w1++ndl, w2++ndl, w3++ndl, w4++[d])
  , loop_loop_step_loop = \(w1,w2,w3,w4) (Z:.():.():.c :.()) -> (w1++ndl, w2++ndl, w3++[c], w4++ndl)
  , loop_loop_step_step = \(w1,w2,w3,w4) (Z:.():.():.c :.d ) -> (w1++ndl, w2++ndl, w3++[c], w4++[d])
  , loop_step_loop_loop = \(w1,w2,w3,w4) (Z:.():.b :.():.()) -> (w1++ndl, w2++[b], w3++ndl, w4++ndl)
  , loop_step_loop_step = \(w1,w2,w3,w4) (Z:.():.b :.():.d ) -> (w1++ndl, w2++[b], w3++ndl, w4++[d])
  , loop_step_step_loop = \(w1,w2,w3,w4) (Z:.():.b :.c :.()) -> (w1++ndl, w2++[b], w3++[c], w4++ndl)
  , loop_step_step_step = \(w1,w2,w3,w4) (Z:.():.b :.c :.d ) -> (w1++ndl, w2++[b], w3++[c], w4++[d])
  , step_loop_loop_loop = \(w1,w2,w3,w4) (Z:.a :.():.():.()) -> (w1++[a], w2++ndl, w3++ndl, w4++ndl)
  , step_loop_loop_step = \(w1,w2,w3,w4) (Z:.a :.():.():.d ) -> (w1++[a], w2++ndl, w3++ndl, w4++[d])
  , step_loop_step_loop = \(w1,w2,w3,w4) (Z:.a :.():.c :.()) -> (w1++[a], w2++ndl, w3++[c], w4++ndl)
  , step_loop_step_step = \(w1,w2,w3,w4) (Z:.a :.():.c :.d ) -> (w1++[a], w2++ndl, w3++[c], w4++[d])
  , step_step_loop_loop = \(w1,w2,w3,w4) (Z:.a :.b :.():.()) -> (w1++[a], w2++[b], w3++ndl, w4++ndl)
  , step_step_loop_step = \(w1,w2,w3,w4) (Z:.a :.b :.():.d ) -> (w1++[a], w2++[b], w3++ndl, w4++[d])
  , step_step_step_loop = \(w1,w2,w3,w4) (Z:.a :.b :.c :.()) -> (w1++[a], w2++[b], w3++[c], w4++ndl)
  , step_step_step_step = \(w1,w2,w3,w4) (Z:.a :.b :.c :.d ) -> (w1++[a], w2++[b], w3++[c], w4++[d])
  , nil_nil_nil_nil = const ([],[],[],[])
  , h         = return . id
  } where ndl = ["-"]
{-# INLINE sAlign #-}

fourWay vowels consonants scores gapOpen i1 i2 i3 i4 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3:.pointL 0 n4), bt) where
  ws = unsafePerformIO (fourWayFill vowels consonants scores gapOpen i1 i2 i3 i4)
  n1 = V.length i1
  n2 = V.length i2
  n3 = V.length i3
  n4 = V.length i4
  bt = backtrack vowels consonants scores gapOpen i1 i2 i3 i4 ws
{-# NOINLINE fourWay #-}

fourWayFill
  :: VU.Vector Char
  -> VU.Vector Char
  -> [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> IO (PA.Unboxed (Z:.PointL:.PointL:.PointL:.PointL) Double)
fourWayFill vowels consonants scores gapOpen i1 i2 i3 i4 = do
  let n1 = V.length i1
  let n2 = V.length i2
  let n3 = V.length i3
  let n4 = V.length i4
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3:.pointL 0 n4) 0
  let w = mTbl (Z:.EmptyT:.EmptyT:.EmptyT:.EmptyT) t'
  fillTable4 $ gFourWay (sScore vowels consonants scores gapOpen) w (chr i1) (chr i2) (chr i3) (chr i4) Empty Empty Empty Empty
  freeze t'
{-# NOINLINE fourWayFill #-}

backtrack
  :: VU.Vector Char
  -> VU.Vector Char
  -> [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> PA.Unboxed (Z:.PointL:.PointL:.PointL:.PointL) Double
  -> [Aligned]
backtrack vowels consonants scores gapOpen i1 i2 i3 i4 tbl = unId . S.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 :.pointL 0 n3 :.pointL 0 n4 where
  n1 = V.length i1
  n2 = V.length i2
  n3 = V.length i3
  n4 = V.length i4
  w :: DefBtTbl Id (Z:.PointL:.PointL:.PointL:.PointL) Double Aligned
  w = btTbl (Z:.EmptyT:.EmptyT:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL:.PointL:.PointL) -> Id (S.Stream Id Aligned))
  (Z:.(_,g)) = gFourWay (sScore vowels consonants scores gapOpen <** sAlign) w (chr i1) (chr i2) (chr i3) (chr i4) Empty Empty Empty Empty
{-# NOINLINE backtrack #-}

{-
test s = fourWay (VU.fromList "aeiou") (VU.fromList $ ['a' .. 'z'] L.\\ "aeiou") [3,1,1,0,0,-1] (-1) s' s' s' s' where
  s' = V.fromList $ L.map (B.pack . (:[])) $ s
-}

