{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

module Linguistics.TwoWay.Bigram
  ( twoWay
  ) where

import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape
import           Data.ByteString.Char8 (ByteString)
import           Data.Strict.Tuple (Pair (..))
import           Data.Vector.Fusion.Util (Id(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.HashTable.IO as H
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as S
import           System.IO.Unsafe (unsafePerformIO)

import           ADP.Fusion
import           ADP.Fusion.Chr
import           ADP.Fusion.Empty
import           ADP.Fusion.Table
import           Data.Array.Repa.Index.Points
import           Data.PrimitiveArray as PA
import           Data.PrimitiveArray.Zero as PA
import           NLP.Alphabet.MultiChar

import           Linguistics.Bigram
import           Linguistics.Common
import           Linguistics.TwoWay.Common



sScore :: Monad m => Double -> Double -> Scores -> STwoWay m Double Double (Maybe InternedMultiChar,InternedMultiChar) ()
sScore dS gapOpen s = STwoWay
  { loop_step = \ww (Z:.():.(mc,c))     -> ww + gapOpen
  , step_loop = \ww (Z:.(mc,c):.())     -> ww + gapOpen
  , step_step = \ww (Z:.(mc,c):.(nd,d)) -> case (mc,nd) of
                                             (Nothing  , Nothing ) -> 0
                                             (Just mc' , Just nd') -> ww + lkup mc' c nd' d
                                             _                     -> -500000
  , nil_nil   = const 0
  , h         = S.foldl' max (-500000)
  } where
    lkup mc' c nd' d = maybe dS id . unsafePerformIO $ H.lookup s (Bigram mc' c :!: Bigram nd' d)
    {-# INLINE lkup #-}
{-# INLINE sScore #-}

-- | Backtrack the alignment

sAlign :: Monad m => STwoWay m Aligned (S.Stream m Aligned) (Maybe InternedMultiChar,InternedMultiChar) ()
sAlign = STwoWay
  { loop_step = \(w1,w2) (Z:.():.(_,c)) -> ( w1 ++ ["-"], w2 ++ [c]   ) -- (w1++padd "" c, w2++prnt c "")
  , step_loop = \(w1,w2) (Z:.(_,c):.()) -> ( w1 ++ [c]  , w2 ++ ["-"] ) -- (w1++prnt c "", w2++padd "" c)
  , step_step = \(w1,w2) (Z:.(_,a):.(_,b)) -> ( w1 ++ [a], w2 ++ [b] ) -- (w1++prnt a b,w2++prnt b a)
  , nil_nil   = const ([],[])
  , h         = return . id
  } where prnt x z = printAligned x [z]
          padd x z = printAlignedPad '-' x [z]
{-# INLINE sAlign #-}

-- | Wrap calculations

twoWay dS gapOpen scores i1 i2 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2), bt) where
  ws = unsafePerformIO (twoWayFill dS gapOpen scores i1 i2)
  n1 = V.length i1
  n2 = V.length i2
  bt = backtrack dS gapOpen scores i1 i2 ws
{-# NOINLINE twoWay #-}

-- | Forward phase

twoWayFill
  :: Double
  -> Double
  -> Scores
  -> V.Vector InternedMultiChar
  -> V.Vector InternedMultiChar
  -> IO (PA.Unboxed (Z:.PointL:.PointL) Double)
twoWayFill dS gapOpen scores i1 i2 = do
  let n1 = V.length i1
  let n2 = V.length i2
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2) 0
  let w = mTbl (Z:.EmptyT:.EmptyT) t'
  fillTable2 $ gTwoWay (sScore dS gapOpen scores) w (chrLeft i1) (chrLeft i2) Empty Empty
  freeze t'
{-# NOINLINE twoWayFill #-}

backtrack
  :: Double
  -> Double
  -> Scores
  -> V.Vector InternedMultiChar
  -> V.Vector InternedMultiChar
  -> PA.Unboxed (Z:.PointL:.PointL) Double
  -> [Aligned]
backtrack dS gapOpen scores i1 i2 tbl = unId . S.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 where
  n1 = V.length i1
  n2 = V.length i2
  w :: DefBtTbl Id (Z:.PointL:.PointL) Double Aligned
  w = btTbl (Z:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL) -> Id (S.Stream Id Aligned))
  (Z:.(_,g)) = gTwoWay (sScore dS gapOpen scores <** sAlign) w (chrLeft i1) (chrLeft i2) Empty Empty
{-# NOINLINE backtrack #-}

