{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

module Linguistics.TwoWay.Simple
  ( twoWay
  ) where

import           Data.Array.Repa.Index
import           Data.Array.Repa.Shape
import           Data.ByteString.Char8 (ByteString)
import           Data.Vector.Fusion.Util (Id(..))
import           GHC.Exts
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Unboxed as VU
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashTable.IO as H

import ADP.Fusion
import ADP.Fusion.Chr
import ADP.Fusion.Empty
import ADP.Fusion.Table
import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA
import NLP.Alphabet.MultiChar

import Linguistics.Common
import Linguistics.Scoring.Simple
import Linguistics.Scoring.SimpleParser (SimpleScoring(..))
import Linguistics.TwoWay.Common



sScore :: Monad m => SimpleScoring -> STwoWay m Double Double InternedMultiChar ()
sScore SimpleScoring {..} = STwoWay -- !vowels !consonants !scores !gapOpen = STwoWay
  { loop_step = \ww (Z:.():.c)     -> ww + gapScore
  , step_loop = \ww (Z:.c:.())     -> ww + gapScore
  , step_step = \ww (Z:.c:.d ) -> ww + (maybe (if c==d then defMatch else defMismatch)
                                              id
                                              . unsafePerformIO $ H.lookup simpleScore (c,d))
  , nil_nil   = const 0
  , h         = S.foldl' max (-500000)
  }
{-# INLINE sScore #-}

sAlign :: Monad m => STwoWay m Aligned (S.Stream m Aligned) InternedMultiChar ()
sAlign = STwoWay
  { loop_step = \(w1,w2) (Z:.():.c) -> ( w1 ++ ["-"], w2 ++ [c]   )
  , step_loop = \(w1,w2) (Z:.c:.()) -> ( w1 ++ [c]  , w2 ++ ["-"] )
  , step_step = \(w1,w2) (Z:.a:.b) ->  ( w1 ++ [a]  , w2 ++ [b]   )
  , nil_nil   = const ([],[])
  , h         = return . id
  }
{-# INLINE sAlign #-}

--twoWay :: VU.Vector Char -> VU.Vector Char -> [Double] -> Double -> V.Vector ByteString -> V.Vector ByteString -> (Double
twoWay simpleScoring i1 i2 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2), bt) where
  ws = unsafePerformIO $ twoWayFill simpleScoring i1 i2
  n1 = V.length i1
  n2 = V.length i2
  bt = backtrack simpleScoring i1 i2 ws
{-# NOINLINE twoWay #-}

twoWayFill
  :: SimpleScoring
  -> V.Vector InternedMultiChar
  -> V.Vector InternedMultiChar
  -> IO (PA.Unboxed (Z:.PointL:.PointL) Double)
twoWayFill simpleScoring i1 i2 = do
  let n1 = V.length i1
  let n2 = V.length i2
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2) 0
  let w = mTbl (Z:.EmptyT:.EmptyT) t'
  fillTable2 $ gTwoWay (sScore simpleScoring) w (chr i1) (chr i2) Empty Empty
  freeze t'
{-# NOINLINE twoWayFill #-}

backtrack
  :: SimpleScoring
  -> V.Vector InternedMultiChar
  -> V.Vector InternedMultiChar
  -> PA.Unboxed (Z:.PointL:.PointL) Double
  -> [Aligned]
backtrack simpleScoring i1 i2 tbl = unId . S.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 where
  n1 = V.length i1
  n2 = V.length i2
  w :: DefBtTbl Id (Z:.PointL:.PointL) Double Aligned
  w = btTbl (Z:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL) -> Id (S.Stream Id Aligned))
  (Z:.(_,g)) = gTwoWay (sScore simpleScoring <** sAlign) w (chr i1) (chr i2) Empty Empty
{-# NOINLINE backtrack #-}

--test s = twoWay (VU.fromList "aeiou") (VU.fromList $ ['a' .. 'z'] L.\\ "aeiou") [3,1,1,0,0,-1] (-1) s' s' where
--  s' = V.fromList $ L.map (B.pack . (:[])) $ s

