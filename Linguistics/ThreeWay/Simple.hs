{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

module Linguistics.ThreeWay.Simple where

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
import Linguistics.ThreeWay.Common



sScore :: Monad m => VU.Vector Char -> VU.Vector Char -> [Double] -> Double -> SThreeWay m Double Double ByteString ()
sScore !vowels !consonants !scores !gapOpen = SThreeWay
  { loop_loop_step = \ww (Z:.():.():.c ) -> ww + gapOpen + gapOpen + 0 -- 0 is the mnemonic for loop/loop match
  , loop_step_loop = \ww (Z:.():.b :.()) -> ww + gapOpen + gapOpen + 0
  , loop_step_step = \ww (Z:.():.b :.c ) -> ww + gapOpen + gapOpen + sm b c
  , step_loop_loop = \ww (Z:.a :.():.()) -> ww + gapOpen + gapOpen + 0
  , step_loop_step = \ww (Z:.a :.():.c ) -> ww + gapOpen + gapOpen + sm a c
  , step_step_loop = \ww (Z:.a :.b :.()) -> ww + gapOpen + gapOpen + sm a b
  , step_step_step = \ww (Z:.a :.b :.c ) -> ww + sm a b + sm a c + sm b c
  , nil_nil_nil    = const 0
  , h              = S.foldl' max (-500000)
  } where sm = scoreMatch vowels consonants scores gapOpen
          {-# INLINE sm #-}
{-# INLINE sScore #-}

sAlign :: Monad m => SThreeWay m Aligned (S.Stream m Aligned) ByteString ()
sAlign = SThreeWay
  { loop_loop_step = \(w1,w2,w3) (Z:.():.():.c ) -> (w1++ndl, w2++ndl, w3++[c]) -- (w1++padd "" c  , w2++padd "" c, w3++prnt c "")
  , loop_step_loop = \(w1,w2,w3) (Z:.():.b :.()) -> (w1++ndl, w2++[b], w3++ndl) -- (w1++padd "" b  , w2++prnt b "", w3++padd "" b)
  , loop_step_step = \(w1,w2,w3) (Z:.():.b :.c ) -> (w1++ndl, w2++[b], w3++[c]) -- (w1++pad2 "" b c, w2++prn2 b c , w3++prn2 c b )
  , step_loop_loop = \(w1,w2,w3) (Z:.a :.():.()) -> (w1++[a], w2++ndl, w3++ndl) -- (w1,w2,w3) -- (w1++padd a ""  , 
  , step_loop_step = \(w1,w2,w3) (Z:.a :.():.c ) -> (w1++[a], w2++ndl, w3++[c]) -- (w1++prnt a c   , w2++pad2 a c , w3++prnt c a )
  , step_step_loop = \(w1,w2,w3) (Z:.a :.b :.()) -> (w1++[a], w2++[b], w3++ndl) -- (w1,w2,w3)
  , step_step_step = \(w1,w2,w3) (Z:.a :.b :.c ) -> (w1++[a], w2++[b], w3++[c]) -- (w1,w2,w3)
  , nil_nil_nil    = const ([],[],[])
  , h              = return . id
  } where ndl = ["-"]
{-# INLINE sAlign #-}

threeWay vowels consonants scores gapOpen i1 i2 i3 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3), bt) where
  ws = unsafePerformIO (threeWayFill vowels consonants scores gapOpen i1 i2 i3)
  n1 = V.length i1
  n2 = V.length i2
  n3 = V.length i3
  bt = backtrack vowels consonants scores gapOpen i1 i2 i3 ws
{-# NOINLINE threeWay #-}

threeWayFill
  :: VU.Vector Char
  -> VU.Vector Char
  -> [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> IO (PA.Unboxed (Z:.PointL:.PointL:.PointL) Double)
threeWayFill vowels consonants scores gapOpen i1 i2 i3 = do
  let n1 = V.length i1
  let n2 = V.length i2
  let n3 = V.length i3
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3) 0
  let w = mTbl (Z:.EmptyT:.EmptyT:.EmptyT) t'
  fillTable3 $ gThreeWay (sScore vowels consonants scores gapOpen) w (chr i1) (chr i2) (chr i3) Empty Empty Empty
  freeze t'
{-# INLINE threeWayFill #-}

backtrack
  :: VU.Vector Char
  -> VU.Vector Char
  -> [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> PA.Unboxed (Z:.PointL:.PointL:.PointL) Double
  -> [Aligned]
backtrack vowels consonants scores gapOpen i1 i2 i3 tbl = unId . S.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 :.pointL 0 n3 where
  n1 = V.length i1
  n2 = V.length i2
  n3 = V.length i3
  w :: DefBtTbl Id (Z:.PointL:.PointL:.PointL) Double Aligned
  w = btTbl (Z:.EmptyT:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL:.PointL) -> Id (S.Stream Id Aligned))
  (Z:.(_,g)) = gThreeWay (sScore vowels consonants scores gapOpen <** sAlign) w (chr i1) (chr i2) (chr i3) Empty Empty Empty
{-# INLINE backtrack #-}

test s = threeWay (VU.fromList "aeiou") (VU.fromList $ ['a' .. 'z'] L.\\ "aeiou") [3,1,1,0,0,-1] (-1) s' s' s' where
  s' = V.fromList $ L.map (B.pack . (:[])) $ s

