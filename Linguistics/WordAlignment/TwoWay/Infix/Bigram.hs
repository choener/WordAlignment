
module Linguistics.WordAlignment.TwoWay.Infix.Bigram where

import           Data.FMList (FMList)
import           Data.Strict.Tuple (Pair (..))
import           Data.Vector.Fusion.Util (Id(..))
import           Data.Vector.Unboxed (Vector)
import qualified Data.FMList as FM
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU

import           ADP.Fusion
import           Data.PrimitiveArray
import           DP.Alignment.Global.Infix2
import           NLP.Scoring.SimpleUnigram
import           NLP.Text.BTI

import           Linguistics.WordAlignment.Bigram
import           Linguistics.WordAlignment.Common
import           Linguistics.WordAlignment.Word (FastChars, fastChar)



type SigT m x r = SigInfix m x r IMCp IMCp

-- |
--
-- TODO check if the score is in the right order!

sScore :: Monad m => SimpleScoring -> Scores -> SigT m Double Double
sScore ss@SimpleScoring{..} bgm = SigInfix
  { align = \ww (Z:.(lb,b):.(lu,u)) ->
              let s  = HM.lookupDefault defMismatch (Bigram lu u :!: Bigram lb b) bgm
              in  ww + scoreUnigram ss b u
  , contL = \ww (Z:.b:._) -> ww + gapExt
  , contU = \ww (Z:._:.u) -> ww + gapExt
  , done  = const 0
  , frSUD = \ww (Z:._:.u) -> ww + preSufOpen
  , frSUI = \ww (Z:._:.u) -> ww + preSufOpen
  , frSUM = \ww (Z:._:.u) -> ww + preSufOpen
  , frUSD = \ww (Z:.b:._) -> ww + preSufOpen
  , frUSI = \ww (Z:.b:._) -> ww + preSufOpen
  , frUSM = \ww (Z:.b:._) -> ww + preSufOpen
  , openL = \ww (Z:.b:._) -> ww + gapOpen
  , openU = \ww (Z:._:.u) -> ww + gapOpen
  , prePU = \ww (Z:._:.u) -> ww + preSufExt
  , preUP = \ww (Z:.b:._) -> ww + preSufExt
  , start = \ww -> ww
  , sufSU = \ww (Z:._:.u) -> ww + preSufExt
  , sufUS = \ww (Z:.b:._) -> ww + preSufExt
  , toPUD = \ww (Z:._:.u) -> ww + preSufOpen + gapOpen
  , toPUI = \ww (Z:._:.u) -> ww + preSufOpen + gapOpen
  , toPUM = \ww (Z:._:.u) -> ww + preSufOpen
  , toUPD = \ww (Z:.b:._) -> ww + preSufOpen + gapOpen
  , toUPI = \ww (Z:.b:._) -> ww + preSufOpen + gapOpen
  , toUPM = \ww (Z:.b:._) -> ww + preSufOpen
  , h     = SM.foldl' max (-888888)
  }
{-# Inline sScore #-}

sBacktrackBuilder :: Monad m => FastChars -> Int -> SimpleScoring -> Scores -> SigT m (FMList B3) [FMList B3]
sBacktrackBuilder !fc !k !ss@SimpleScoring{..} bgm = SigInfix
  { align = \ww (Z:.(_,b):.(_,u)) -> ww `FM.snoc` ( fastChar fc u
                                                  , fastChar fc b
                                                  , TF.left k ' ' . TF.fixed 1 $ scoreUnigram ss b u
                                                  )
  , contL = dow gapExt
  , contU = upp gapExt
  , done  = mempty
  , frSUD = upp preSufOpen
  , frSUI = upp preSufOpen
  , frSUM = upp preSufOpen
  , frUSD = dow preSufOpen
  , frUSI = dow preSufOpen
  , frUSM = dow preSufOpen
  , openL = dow gapOpen
  , openU = upp gapOpen
  , prePU = upp preSufExt
  , preUP = dow preSufExt
  , start = \ww -> ww
  , sufSU = upp preSufExt
  , sufUS = dow preSufExt
  , toPUD = upp (preSufOpen + gapOpen)
  , toPUI = upp (preSufOpen + gapOpen)
  , toPUM = upp preSufOpen
  , toUPD = dow (preSufOpen + gapOpen)
  , toUPI = dow (preSufOpen + gapOpen)
  , toUPM = dow preSufOpen
  , h     = SM.toList
  } where upp s ww (Z:._:.(_,u)) = ww `FM.snoc` ( fastChar fc u
                                                , fastChar fc "-"
                                                , TF.left k ' ' $ TF.fixed 1 s
                                                )
          {-# Inline upp #-}
          dow s ww (Z:.(_,b):._) = ww `FM.snoc` ( fastChar fc "-"
                                                , fastChar fc b
                                                , TF.left k ' ' $ TF.fixed 1 s
                                                )
          {-# Inline dow #-}
{-# Inline sBacktrackBuilder #-}

type F = TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double
type B = TwITblBt Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double Id Id (FM.FMList B3)

alignInfixForward
  :: SimpleScoring
  -> Scores
  -> Vector IMCp
  -> Vector IMCp
  -> Z:.F:.F:.F:.F:.F:.F:.F:.F
alignInfixForward simpleS bgm i1 i2 = {-# SCC "alignInfixForward" #-} mutateTablesDefault $
  gInfix (sScore simpleS bgm)
    (ITbl 1 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))  -- DD
    (ITbl 1 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))  -- II
    (ITbl 1 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))  -- MM
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))  -- PU prefix
    (ITbl 3 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))  -- SS    start
    (ITbl 2 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))  -- SU   suffix
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))  -- UP prefix
    (ITbl 2 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))  -- US   suffix
    (chr i1) (chr i2)
  where n1 = VU.length i1
        n2 = VU.length i2
{-# NoInline alignInfixForward #-}

alignInfixBacktrack
  :: FastChars
  -> Int
  -> SimpleScoring
  -> Scores
  -> Vector IMCp
  -> Vector IMCp
  -> Z:.F:.F:.F:.F:.F:.F:.F:.F
  -> [[B3]]
alignInfixBacktrack fc width simpleS bgm i1 i2 (Z:.dd:.ii:.mm:.pu:.ss:.su:.up:.us) = {-# SCC "alignInfixBacktrack" #-} L.map FM.toList . unId $ axiom ss'
  where (Z:._:._:._:._:.ss':._:._:._) = gInfix (sScore simpleS bgm <|| sBacktrackBuilder fc width simpleS bgm)
                                          (toBacktrack dd (undefined :: Id a -> Id a))
                                          (toBacktrack ii (undefined :: Id a -> Id a))
                                          (toBacktrack mm (undefined :: Id a -> Id a))
                                          (toBacktrack pu (undefined :: Id a -> Id a))
                                          (toBacktrack ss (undefined :: Id a -> Id a))
                                          (toBacktrack su (undefined :: Id a -> Id a))
                                          (toBacktrack up (undefined :: Id a -> Id a))
                                          (toBacktrack us (undefined :: Id a -> Id a))
                                          (chr i1) (chr i2)
                                          :: Z:.B:.B:.B:.B:.B:.B:.B:.B
{-# NoInline alignInfixBacktrack #-}

alignInfix
  :: FastChars
  -> Int
  -> SimpleScoring
  -> Scores
  -> Vector BTI
  -> Vector BTI
  -> Int
  -> (Double , [[B3]])
alignInfix fc width simpleS bgm i1' i2' k = {-# SCC "alignInfix" #-} (d, take k bs)
  where d = unId $ axiom ss
        fwd@(Z:.dd:.ii:.mm:.pu:.ss:.su:.up:.us) = alignInfixForward simpleS bgm i1 i2
        bs = alignInfixBacktrack fc width simpleS bgm i1 i2 fwd
        mkI m = VU.zip m (VU.tail m)
        i1 = mkI i1'
        i2 = mkI i2'
{-# NoInline alignInfix #-}

