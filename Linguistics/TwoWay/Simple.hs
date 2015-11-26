{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

module Linguistics.TwoWay.Simple where

import           Data.ByteString.Char8 (ByteString)
import           Data.Vector.Fusion.Util (Id(..))
import           GHC.Exts
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Fusion.Stream.Monadic as SM
--import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed (Vector)
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Sequence (Seq)
import           Data.FMList (FMList)
import           Data.Text (Text)
import           Data.Stringable (toText)

import           ADP.Fusion
import           Data.PrimitiveArray
import           DP.Alignment.Global.Tapes2
import           NLP.Scoring.SimpleUnigram
import           NLP.Text.BTI

import Linguistics.Common
--import Linguistics.TwoWay.Common


--type IMC = InternedMultiChar
type SigT m x r = SigGlobal m x r BTI BTI

sScore :: Monad m => SimpleScoring -> SigT m Double Double
sScore ss@SimpleScoring{..} = SigGlobal
  { delin = \ww (Z:.c:._)     -> ww + gapScore
  , indel = \ww (Z:._:.c)     -> ww + gapScore
  , align = \ww (Z:.l:.u) -> ww + scoreUnigram ss l u
  , done = const 0
  , h         = SM.foldl' max (-888888)
  }
{-# Inline sScore #-}

sBacktrack :: Monad m => SigT m (FMList (BTI,BTI)) [FMList (BTI,BTI)]
sBacktrack = backtrack "-" "-"
{-# Inline sBacktrack #-}

-- | Create a backtracking function
--
-- TODO includes scores as well?

sBacktrackFun :: Monad m => SigT m (FMList [Text]) [FMList [Text]]
sBacktrackFun = backtrackFun f g "-" "-" where
  f c d = [toText c, toText d]
  g c d = [toText c, toText d]

alignGlobal :: SimpleScoring -> Int -> Vector BTI -> Vector BTI -> (Double,[[[Text]]])
alignGlobal scoring k i1 i2 = (d, take k bs) where
  n1 = VU.length i1 ; n2 = VU.length i2
  !(Z:.t) = alignGlobalForward scoring i1 i2
  d = unId $ axiom t
  bs = alignGlobalBacktrack scoring i1 i2 t
{-# NoInline alignGlobal #-}

alignGlobalForward :: SimpleScoring -> Vector BTI -> Vector BTI -> Z:.ITbl Id Unboxed (Z:.PointL I:.PointL I) Double
alignGlobalForward scoring i1 i2 = {-# SCC "alignGlobalForward" #-} mutateTablesDefault $
  gGlobal (sScore scoring)
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))
    (chr i1) (chr i2)
  where n1 = VU.length i1
        n2 = VU.length i2
{-# NoInline alignGlobalForward #-}

alignGlobalBacktrack :: SimpleScoring -> Vector BTI -> Vector BTI -> ITbl Id Unboxed (Z:.PointL I:.PointL I) Double -> [[[Text]]]
alignGlobalBacktrack scoring i1 i2 t = {-# SCC "alignGlobalBacktrack" #-} L.map runBacktrack . unId $ axiom b
  where (Z:.b) = gGlobal (sScore scoring <|| sBacktrackFun) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline alignGlobalBacktrack #-}

-- | Decoupling the forward phase for CORE observation.

{-
alignGlobalForward :: Vector IMC -> Vector IMC -> Z:.(ITbl Id Unboxed (Z:.PointL:.PointL) Double)
alignGlobalForward i1 i2 = let n1 = VU.length i1; n2 = VU.length i2 in mutateTablesDefault $
  gGlobal score
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))
    (chr i1) (chr i2)
{-# NoInline runNeedlemanWunschForward #-}
-}



{-
sAlign :: Monad m => STwoWay m Aligned (S.Stream m Aligned) InternedMultiChar ()
sAlign = STwoWay
  { loop_step = \[w1,w2] (Z:.():.c) -> [ "-" : w1 , c   : w2 ]
  , step_loop = \[w1,w2] (Z:.c:.()) -> [ c   : w1 , "-" : w2 ]
  , step_step = \[w1,w2] (Z:.a:.b) ->  [ a   : w1 , b   : w2 ]
  , nil_nil   = const [[],[]]
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
  let w = mTblD (Z:.EmptyOk:.EmptyOk) t'
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
  w = btTblD (Z:.EmptyOk:.EmptyOk) tbl (g :: (Z:.PointL:.PointL) -> Id (S.Stream Id Aligned))
  (Z:.(_,g)) = gTwoWay (sScore simpleScoring <** sAlign) w (chr i1) (chr i2) Empty Empty
{-# NOINLINE backtrack #-}

--test s = twoWay (VU.fromList "aeiou") (VU.fromList $ ['a' .. 'z'] L.\\ "aeiou") [3,1,1,0,0,-1] (-1) s' s' where
--  s' = V.fromList $ L.map (B.pack . (:[])) $ s

-}

