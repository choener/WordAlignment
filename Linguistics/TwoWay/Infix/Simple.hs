
module Linguistics.TwoWay.Infix.Simple where

import           Data.ByteString.Char8 (ByteString)
import           Data.FMList (FMList)
import           Data.Sequence (Seq)
import           Data.Stringable (toText)
import           Data.Text (Text)
import           Data.Vector.Fusion.Util (Id(..))
import           Data.Vector.Unboxed (Vector)
import           GHC.Exts
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.Lazy.Builder as TLB
import           Data.FMList (FMList)
import qualified Data.FMList as FM
import qualified Data.Text.Format as TF

import           ADP.Fusion
import           Data.PrimitiveArray
import           DP.Alignment.Global.Infix2
import           NLP.Scoring.SimpleUnigram
import           NLP.Text.BTI

import           Linguistics.Common



type B3 = (TLB.Builder, TLB.Builder, TLB.Builder)

data InfixScoring = InfixScoring
  { infixOpen :: !Double
  , infixExt  :: !Double
  }

type SigT m x r = SigInfix m x r BTI BTI

sScore :: Monad m => InfixScoring -> SimpleScoring -> SigT m Double Double
sScore InfixScoring{..} ss@SimpleScoring{..} = SigInfix
  { align = \ww (Z:.b:.u) -> ww + scoreUnigram ss b u
  , contL = \ww (Z:.b:._) -> ww + gapExtend
  , contU = \ww (Z:._:.u) -> ww + gapExtend
  , done  = const 0
  , frSUD = \ww (Z:._:.u) -> ww + infixOpen
  , frSUI = \ww (Z:._:.u) -> ww + infixOpen
  , frSUM = \ww (Z:._:.u) -> ww + infixOpen
  , frUSD = \ww (Z:.b:._) -> ww + infixOpen
  , frUSI = \ww (Z:.b:._) -> ww + infixOpen
  , frUSM = \ww (Z:.b:._) -> ww + infixOpen
  , openL = \ww (Z:.b:._) -> ww + gapOpen
  , openU = \ww (Z:._:.u) -> ww + gapOpen
  , prePU = \ww (Z:._:.u) -> ww + infixExt
  , preUP = \ww (Z:.b:._) -> ww + infixExt
  , start = \ww -> ww
  , sufSU = \ww (Z:._:.u) -> ww + infixExt
  , sufUS = \ww (Z:.b:._) -> ww + infixExt
  , toPUD = \ww (Z:._:.u) -> ww + infixOpen + gapOpen
  , toPUI = \ww (Z:._:.u) -> ww + infixOpen + gapOpen
  , toPUM = \ww (Z:._:.u) -> ww + infixOpen
  , toUPD = \ww (Z:.b:._) -> ww + infixOpen + gapOpen
  , toUPI = \ww (Z:.b:._) -> ww + infixOpen + gapOpen
  , toUPM = \ww (Z:.b:._) -> ww + infixOpen
  , h     = SM.foldl' max (-888888)
  }
{-# Inline sScore #-}

sBacktrackBuilder :: Monad m => Int -> InfixScoring -> SimpleScoring -> SigT m (FMList B3) [FMList B3]
sBacktrackBuilder !k InfixScoring{..} ss@SimpleScoring{..} = SigInfix
  { align = \ww (Z:.b:.u) -> ww `FM.snoc` ( TF.left k ' ' . TLB.fromText $ toText u
                                          , TF.left k ' ' . TLB.fromText $ toText b
                                          , TF.left k ' ' . TF.fixed 1 $ scoreUnigram ss b u
                                          )
  , contL = dow gapExtend
  , contU = upp gapExtend
  , done  = mempty
  , frSUD = upp infixOpen
  , frSUI = upp infixOpen
  , frSUM = upp infixOpen
  , frUSD = dow infixOpen
  , frUSI = dow infixOpen
  , frUSM = dow infixOpen
  , openL = dow gapOpen
  , openU = upp gapOpen
  , prePU = upp infixExt
  , preUP = dow infixExt
  , start = \ww -> ww
  , sufSU = upp infixExt
  , sufUS = dow infixExt
  , toPUD = upp (infixOpen + gapOpen)
  , toPUI = upp (infixOpen + gapOpen)
  , toPUM = upp infixOpen
  , toUPD = dow (infixOpen + gapOpen)
  , toUPI = dow (infixOpen + gapOpen)
  , toUPM = dow infixOpen
  , h     = SM.toList
  } where upp s ww (Z:._:.u) = ww `FM.snoc` ( TF.left k ' ' . TLB.fromText $ toText (u::BTI)
                                            , TF.left k ' ' ("-" :: TLB.Builder)
                                            , TF.left k ' ' $ TF.fixed 1 s
                                            )
          {-# Inline upp #-}
          dow s ww (Z:.b:._) = ww `FM.snoc` ( TF.left k ' ' ("-" :: TLB.Builder)
                                            , TF.left k ' ' . TLB.fromText $ toText (b::BTI)
                                            , TF.left k ' ' $ TF.fixed 1 s
                                            )
          {-# Inline dow #-}
{-# Inline sBacktrackBuilder #-}

type F = TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double
type B = TwITblBt Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double Id Id (FM.FMList B3)

alignInfixForward
  :: InfixScoring
  -> SimpleScoring
  -> Vector BTI
  -> Vector BTI
  -> Z:.F:.F:.F:.F:.F:.F:.F:.F
alignInfixForward infixS simpleS i1 i2 = {-# SCC "alignInfixForward" #-} mutateTablesDefault $
  gInfix (sScore infixS simpleS)
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
  :: Int
  -> InfixScoring
  -> SimpleScoring
  -> Vector BTI
  -> Vector BTI
  -> Z:.F:.F:.F:.F:.F:.F:.F:.F
  -> [[B3]]
alignInfixBacktrack width infixS simpleS i1 i2 (Z:.dd:.ii:.mm:.pu:.ss:.su:.up:.us) = {-# SCC "alignInfixBacktrack" #-} L.map FM.toList . unId $ axiom ss'
  where (Z:._:._:._:._:.ss':._:._:._) = gInfix (sScore infixS simpleS <|| sBacktrackBuilder width infixS simpleS)
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
  :: Int
  -> InfixScoring
  -> SimpleScoring
  -> Vector BTI
  -> Vector BTI
  -> Int
  -> (Double , [[B3]])
alignInfix width infixS simpleS i1 i2 k = {-# SCC "alignInfix" #-} (d, take k bs)
  where d = unId $ axiom ss
        fwd@(Z:.dd:.ii:.mm:.pu:.ss:.su:.up:.us) = alignInfixForward infixS simpleS i1 i2
        bs = alignInfixBacktrack width infixS simpleS i1 i2 fwd
{-# NoInline alignInfix #-}

