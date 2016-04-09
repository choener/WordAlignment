
module Linguistics.WordAlignment.TwoWay.Global.Simple where

import           Data.ByteString.Char8 (ByteString)
import           Data.FMList (FMList)
import           Data.Sequence (Seq)
import           Data.Stringable (toText)
import           Data.Text (Text)
import           Data.Vector.Fusion.Util (Id(..))
import           Data.Vector.Unboxed (Vector)
import           GHC.Exts
import qualified Data.ByteString.Char8 as B
import qualified Data.FMList as FM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text.Format as TF

import           ADP.Fusion
import           Data.PrimitiveArray
import           DP.Alignment.Global.Linear2
import           NLP.Scoring.SimpleUnigram
import           NLP.Text.BTI

import           Linguistics.WordAlignment.Common
import           Linguistics.WordAlignment.Word (FastChars, fastChar)



type SigT m x r = SigGlobal m x r BTI BTI

-- |

sScore :: Monad m => SimpleScoring -> SigT m Double Double
sScore ss@SimpleScoring{..} = SigGlobal
  { delin = \ww (Z:.c:._)     -> ww + gapScore
  , indel = \ww (Z:._:.c)     -> ww + gapScore
  , align = \ww (Z:.l:.u) -> ww + scoreUnigram ss l u
  , done = const 0
  , h         = SM.foldl' max (-888888)
  }
{-# Inline sScore #-}

-- |

sBacktrackBuilder :: Monad m => FastChars -> Int -> SimpleScoring -> SigT m (FMList B3) [FMList B3]
sBacktrackBuilder !fc !k !ss@SimpleScoring{..} = SigGlobal
  { delin = \ww (Z:.b:._) -> ww `FM.snoc` ( fastChar fc "-"
                                          , fastChar fc b
                                          , TF.left k ' ' $ TF.fixed 1 gapScore
                                          )
  , indel = \ww (Z:._:.u) -> ww `FM.snoc` ( fastChar fc u
                                          , fastChar fc "-"
                                          , TF.left k ' ' $ TF.fixed 1 gapScore
                                          )
  , align = \ww (Z:.b:.u) -> ww `FM.snoc` ( fastChar fc u
                                          , fastChar fc b
                                          , TF.left k ' ' . TF.fixed 1 $ scoreUnigram ss b u
                                          )
  , done = const mempty
  , h = SM.toList
  }
{-# Inline sBacktrackBuilder #-}

-- |

alignGlobalForward :: SimpleScoring -> Vector BTI -> Vector BTI -> Z:.TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double
alignGlobalForward scoring i1 i2 = {-# SCC "alignGlobalForward" #-} mutateTablesDefault $
  gGlobal (sScore scoring)
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))
    (chr i1) (chr i2)
  where n1 = VU.length i1
        n2 = VU.length i2
{-# NoInline alignGlobalForward #-}

-- |

alignGlobalBacktrack :: FastChars -> Int -> SimpleScoring -> Vector BTI -> Vector BTI -> TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double -> [[B3]]
alignGlobalBacktrack fc width scoring i1 i2 t = {-# SCC "alignGlobalBacktrack" #-} L.map FM.toList . unId $ axiom b
  where (Z:.b) = gGlobal (sScore scoring <|| sBacktrackBuilder fc width scoring) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline alignGlobalBacktrack #-}

-- |

alignGlobal :: FastChars -> Int -> SimpleScoring -> Int -> Vector BTI -> Vector BTI -> (Double,[[B3]])
alignGlobal fc width scoring k i1 i2 = (d, take k bs) where
  n1 = VU.length i1 ; n2 = VU.length i2
  !(Z:.t) = alignGlobalForward scoring i1 i2
  d = unId $ axiom t
  bs = alignGlobalBacktrack fc width scoring i1 i2 t
{-# NoInline alignGlobal #-}

