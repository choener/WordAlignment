{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

module Linguistics.FourWay.Bigram 
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
import qualified Data.HashTable.IO as H
import Data.Strict.Tuple (Pair (..))

import ADP.Fusion
import ADP.Fusion.Chr
import ADP.Fusion.Empty
import ADP.Fusion.Table
import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import Linguistics.Bigram
import Linguistics.FourWay.Common



sScore :: Monad m => Double -> Double -> (Scores,Scores,Scores,Scores,Scores,Scores) -> SFourWay m Double Double (Maybe ByteString,ByteString) ()
sScore dS gapOpen (sab,sac,sad,sbc,sbd,scd) = SFourWay
  { loop_loop_loop_step = \ww (Z:.():.():.():.d ) -> ww + 3 * gapOpen + 3 * 0 -- each function has 6 elements + ww
  , loop_loop_step_loop = \ww (Z:.():.():.c :.()) -> ww + 3 * gapOpen + 3 * 0
  , loop_loop_step_step = \ww (Z:.():.():.c :.d ) -> ww + 4 * gapOpen + 1 * 0 + lkup scd c d
  , loop_step_loop_loop = \ww (Z:.():.b :.():.()) -> ww + 3 * gapOpen + 3 * 0
  , loop_step_loop_step = \ww (Z:.():.b :.():.d ) -> ww + 4 * gapOpen + 1 * 0 + lkup sbd b d
  , loop_step_step_loop = \ww (Z:.():.b :.c :.()) -> ww + 4 * gapOpen + 1 * 0 + lkup sbc b c
  , loop_step_step_step = \ww (Z:.():.b :.c :.d ) -> ww + 3 * gapOpen + 0 * 0 + lkup sbc b c + lkup sbd b d + lkup scd c d
  , step_loop_loop_loop = \ww (Z:.a :.():.():.()) -> ww + 3 * gapOpen + 3 * 0
  , step_loop_loop_step = \ww (Z:.a :.():.():.d ) -> ww + 4 * gapOpen + 1 * 0 + lkup sad a d
  , step_loop_step_loop = \ww (Z:.a :.():.c :.()) -> ww + 4 * gapOpen + 1 * 0 + lkup sac a c
  , step_loop_step_step = \ww (Z:.a :.():.c :.d ) -> ww + 3 * gapOpen + 0 * 0 + lkup sac a c + lkup sad a d + lkup scd c d
  , step_step_loop_loop = \ww (Z:.a :.b :.():.()) -> ww + 4 * gapOpen + 1 * 0 + lkup sab a b
  , step_step_loop_step = \ww (Z:.a :.b :.():.d ) -> ww + 3 * gapOpen + 0 * 0 + lkup sab a b + lkup sad a d + lkup sbd b d
  , step_step_step_loop = \ww (Z:.a :.b :.c :.()) -> ww + 3 * gapOpen + 0 * 0 + lkup sab a b + lkup sac a c + lkup sbc b c
  , step_step_step_step = \ww (Z:.a :.b :.c :.d ) -> ww + 0 * gapOpen + 0 * 0 + lkup sab a b + lkup sac a c + lkup sad a d + lkup sbc b c + lkup sbd b d + lkup scd c d
  , nil_nil_nil_nil = const 0
  , h         = S.foldl' max (-500000)
  } where lkup s (Nothing,   _) (Nothing,   _) = 0
          lkup s ((Just mx), x) ((Just my), y) = maybe dS id . unsafePerformIO $ H.lookup s (Bigram mx x :!: Bigram my y)
          lkup s _              _              = -500000
          {-# INLINE lkup #-}
{-# INLINE sScore #-}

sAlign :: Monad m => SFourWay m Aligned (S.Stream m Aligned) (Maybe ByteString, ByteString) ()
sAlign = SFourWay
  { loop_loop_loop_step = \(w1,w2,w3,w4) (Z:.()   :.()   :.()   :.(_,d)) -> (w1++ndl, w2++ndl, w3++ndl, w4++[d])
  , loop_loop_step_loop = \(w1,w2,w3,w4) (Z:.()   :.()   :.(_,c):.()   ) -> (w1++ndl, w2++ndl, w3++[c], w4++ndl)
  , loop_loop_step_step = \(w1,w2,w3,w4) (Z:.()   :.()   :.(_,c):.(_,d)) -> (w1++ndl, w2++ndl, w3++[c], w4++[d])
  , loop_step_loop_loop = \(w1,w2,w3,w4) (Z:.()   :.(_,b):.()   :.()   ) -> (w1++ndl, w2++[b], w3++ndl, w4++ndl)
  , loop_step_loop_step = \(w1,w2,w3,w4) (Z:.()   :.(_,b):.()   :.(_,d)) -> (w1++ndl, w2++[b], w3++ndl, w4++[d])
  , loop_step_step_loop = \(w1,w2,w3,w4) (Z:.()   :.(_,b):.(_,c):.()   ) -> (w1++ndl, w2++[b], w3++[c], w4++ndl)
  , loop_step_step_step = \(w1,w2,w3,w4) (Z:.()   :.(_,b):.(_,c):.(_,d)) -> (w1++ndl, w2++[b], w3++[c], w4++[d])
  , step_loop_loop_loop = \(w1,w2,w3,w4) (Z:.(_,a):.()   :.()   :.()   ) -> (w1++[a], w2++ndl, w3++ndl, w4++ndl)
  , step_loop_loop_step = \(w1,w2,w3,w4) (Z:.(_,a):.()   :.()   :.(_,d)) -> (w1++[a], w2++ndl, w3++ndl, w4++[d])
  , step_loop_step_loop = \(w1,w2,w3,w4) (Z:.(_,a):.()   :.(_,c):.()   ) -> (w1++[a], w2++ndl, w3++[c], w4++ndl)
  , step_loop_step_step = \(w1,w2,w3,w4) (Z:.(_,a):.()   :.(_,c):.(_,d)) -> (w1++[a], w2++ndl, w3++[c], w4++[d])
  , step_step_loop_loop = \(w1,w2,w3,w4) (Z:.(_,a):.(_,b):.()   :.()   ) -> (w1++[a], w2++[b], w3++ndl, w4++ndl)
  , step_step_loop_step = \(w1,w2,w3,w4) (Z:.(_,a):.(_,b):.()   :.(_,d)) -> (w1++[a], w2++[b], w3++ndl, w4++[d])
  , step_step_step_loop = \(w1,w2,w3,w4) (Z:.(_,a):.(_,b):.(_,c):.()   ) -> (w1++[a], w2++[b], w3++[c], w4++ndl)
  , step_step_step_step = \(w1,w2,w3,w4) (Z:.(_,a):.(_,b):.(_,c):.(_,d)) -> (w1++[a], w2++[b], w3++[c], w4++[d])
  , nil_nil_nil_nil = const ([],[],[],[])
  , h         = return . id
  } where ndl = ["-"]
{-# INLINE sAlign #-}

fourWay dS gapOpen scores i1 i2 i3 i4 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3:.pointL 0 n4), bt) where
  ws = unsafePerformIO (fourWayFill dS gapOpen scores i1 i2 i3 i4)
  n1 = V.length i1
  n2 = V.length i2
  n3 = V.length i3
  n4 = V.length i4
  bt = backtrack dS gapOpen scores i1 i2 i3 i4 ws
{-# NOINLINE fourWay #-}

fourWayFill
  :: Double
  -> Double
  -> (Scores,Scores,Scores,Scores,Scores,Scores)
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> IO (PA.Unboxed (Z:.PointL:.PointL:.PointL:.PointL) Double)
fourWayFill dS gapOpen scores i1 i2 i3 i4 = do
  let n1 = V.length i1
  let n2 = V.length i2
  let n3 = V.length i3
  let n4 = V.length i4
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3:.pointL 0 n4) 0
  let w = mTbl (Z:.EmptyT:.EmptyT:.EmptyT:.EmptyT) t'
  fillTable4 $ gFourWay (sScore dS gapOpen scores ) w (chrLeft i1) (chrLeft i2) (chrLeft i3) (chrLeft i4) Empty Empty Empty Empty
  freeze t'
{-# INLINE fourWayFill #-}

backtrack
  :: Double
  -> Double
  -> (Scores,Scores,Scores,Scores,Scores,Scores)
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> PA.Unboxed (Z:.PointL:.PointL:.PointL:.PointL) Double
  -> [Aligned]
backtrack dS gapOpen scores i1 i2 i3 i4 tbl = unId . S.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 :.pointL 0 n3 :.pointL 0 n4 where
  n1 = V.length i1
  n2 = V.length i2
  n3 = V.length i3
  n4 = V.length i4
  w :: DefBtTbl Id (Z:.PointL:.PointL:.PointL:.PointL) Double Aligned
  w = btTbl (Z:.EmptyT:.EmptyT:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL:.PointL:.PointL) -> Id (S.Stream Id Aligned))
  (Z:.(_,g)) = gFourWay (sScore dS gapOpen scores <** sAlign) w (chrLeft i1) (chrLeft i2) (chrLeft i3) (chrLeft i4) Empty Empty Empty Empty
{-# INLINE backtrack #-}

