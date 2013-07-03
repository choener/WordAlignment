{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

module Linguistics.ThreeWay.Bigram where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.ByteString.Char8 (ByteString)
import Data.Strict.Tuple (Pair (..))
import Data.Vector.Fusion.Util (Id(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.HashTable.IO as H
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as S
import System.IO.Unsafe (unsafePerformIO)

import ADP.Fusion
import ADP.Fusion.Chr
import ADP.Fusion.Empty
import ADP.Fusion.Table
import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import Linguistics.Bigram
import Linguistics.Common
import Linguistics.ThreeWay.Common



sScore :: Monad m => Double -> Double -> (Scores,Scores,Scores) -> SThreeWay m Double Double (Maybe ByteString,ByteString) ()
sScore dS gapOpen (sab,sac,sbc) = SThreeWay
  { loop_loop_step = \ww (Z:.():.():.c ) -> ww + gapOpen + gapOpen + 0 -- 0 is the mnemonic for loop/loop match
  , loop_step_loop = \ww (Z:.():.b :.()) -> ww + gapOpen + gapOpen + 0
  , loop_step_step = \ww (Z:.():.(mb,b) :.(mc,c) ) -> ww + gapOpen + gapOpen + lkup sbc mb b mc c
  , step_loop_loop = \ww (Z:.a :.():.()) -> ww + gapOpen + gapOpen + 0
  , step_loop_step = \ww (Z:.(ma,a) :.():.(mc,c) ) -> ww + gapOpen + gapOpen + lkup sac ma a mc c
  , step_step_loop = \ww (Z:.(ma,a) :.(mb,b) :.()) -> ww + gapOpen + gapOpen + lkup sab ma a mb b
  , step_step_step = \ww (Z:.(ma,a) :.(mb,b) :.(mc,c) ) -> ww + lkup sab ma a mb b + lkup sac ma a mc c + lkup sbc mb b mc c
  , nil_nil_nil    = const 0
  , h              = S.foldl' max (-500000)
  } where lkup s Nothing   _ Nothing   _ = 0
          lkup s (Just mx) x (Just my) y = maybe dS id . unsafePerformIO $ H.lookup s (Bigram mx x :!: Bigram my y)
          lkup s _         _ _         _ = -500000
          {-# INLINE lkup #-}
{-# INLINE sScore #-}

-- | Backtrack the alignment

sAlign :: Monad m => SThreeWay m Aligned (S.Stream m Aligned) (Maybe ByteString,ByteString) ()
sAlign = SThreeWay
  { loop_loop_step = \(w1,w2,w3) (Z:.()   :.()   :.(_,c)) -> (w1++ndl, w2++ndl, w3++[c])
  , loop_step_loop = \(w1,w2,w3) (Z:.()   :.(_,b):.()   ) -> (w1++ndl, w2++[b], w3++ndl)
  , loop_step_step = \(w1,w2,w3) (Z:.()   :.(_,b):.(_,c)) -> (w1++ndl, w2++[b], w3++[c])
  , step_loop_loop = \(w1,w2,w3) (Z:.(_,a):.()   :.()   ) -> (w1++[a], w2++ndl, w3++ndl)
  , step_loop_step = \(w1,w2,w3) (Z:.(_,a):.()   :.(_,c)) -> (w1++[a], w2++ndl, w3++[c])
  , step_step_loop = \(w1,w2,w3) (Z:.(_,a):.(_,b):.()   ) -> (w1++[a], w2++[b], w3++ndl)
  , step_step_step = \(w1,w2,w3) (Z:.(_,a):.(_,b):.(_,c)) -> (w1++[a], w2++[b], w3++[c])
  , nil_nil_nil    = const ([],[],[])
  , h              = return . id
  } where ndl = ["-"]
{-# INLINE sAlign #-}


-- | Wrap calculations

threeWay dS gapOpen scores i1 i2 i3 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3), bt) where
  ws = unsafePerformIO (threeWayFill dS gapOpen scores i1 i2 i3)
  n1 = V.length i1
  n2 = V.length i2
  n3 = V.length i3
  bt = backtrack dS gapOpen scores i1 i2 i3 ws
{-# NOINLINE threeWay #-}

-- | Forward phase

threeWayFill
  :: Double
  -> Double
  -> (Scores,Scores,Scores)
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> IO (PA.Unboxed (Z:.PointL:.PointL:.PointL) Double)
threeWayFill dS gapOpen scores i1 i2 i3 = do
  let n1 = V.length i1
  let n2 = V.length i2
  let n3 = V.length i3
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3) 0
  let w = mTbl (Z:.EmptyT:.EmptyT:.EmptyT) t'
  fillTable3 $ gThreeWay (sScore dS gapOpen scores) w (chrLeft i1) (chrLeft i2) (chrLeft i3) Empty Empty Empty
  freeze t'
{-# INLINE threeWayFill #-}

backtrack
  :: Double
  -> Double
  -> (Scores,Scores,Scores)
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> PA.Unboxed (Z:.PointL:.PointL:.PointL) Double
  -> [Aligned]
backtrack dS gapOpen scores i1 i2 i3 tbl = unId . S.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2:.pointL 0 n3 where
  n1 = V.length i1
  n2 = V.length i2
  n3 = V.length i3
  w :: DefBtTbl Id (Z:.PointL:.PointL:.PointL) Double Aligned
  w = btTbl (Z:.EmptyT:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL:.PointL) -> Id (S.Stream Id Aligned))
  (Z:.(_,g)) = gThreeWay (sScore dS gapOpen scores <** sAlign) w (chrLeft i1) (chrLeft i2) (chrLeft i3) Empty Empty Empty

