
module Linguistics.WordAlignment.TwoWay.Global.Bigram where

import           Data.ByteString.Char8 (ByteString)
import           Data.FMList (FMList)
import qualified Data.FMList as FM
import           Data.Sequence (Seq)
import           Data.Strict.Tuple (Pair (..))
import           Data.Text (Text,pack)
import           Data.Vector.Fusion.Util (Id(..))
import           Data.Vector.Unboxed (Vector)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Text.Printf
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.ByteString.Builder as BB

import           ADP.Fusion.Point
import           Data.PrimitiveArray
import           DP.Seq.Align.Global.Linear2
import           NLP.Scoring.SimpleUnigram
import           NLP.Text.BTI

import           Linguistics.WordAlignment.Common
import           Linguistics.WordAlignment.FastLookups
import           Linguistics.WordAlignment.Bigram
import           Linguistics.WordAlignment.Word (FastChars, fastChar, wordToBigramVector)



type IMC = BTI
type SigT m x r = SigGlobal m x r IMCp IMCp

-- |

sScore :: Monad m => SimpleScoring -> Scores -> SigT m Double Double
sScore !ss@SimpleScoring{..} !bgm = SigGlobal
  { delin = \ww (Z:.c     :._     ) -> ww + gapScore
  , indel = \ww (Z:._     :.c     ) -> ww + gapScore
  , align = \ww (Z:.(lp,l):.(up,u)) -> ww + lkup up u lp l
  , done = const 0
  , h         = SM.foldl' max (-888888)
  } where
    lkup mc' c nd' d = {-# SCC "lkup" #-} HM.lookupDefault defMismatch (Bigram mc' c :!: Bigram nd' d) bgm
    {-# Inline lkup #-}
{-# INLINE sScore #-}

-- |

sBacktrackBuilder
  :: Monad m
  => FastChars
  -> FastDoubles
  -> Int
  -> SimpleScoring
  -> Scores
  -> SigT m (FMList B3) [FMList B3]
sBacktrackBuilder !fc !fd !k !ss@SimpleScoring{..} !bgm = SigGlobal
  { delin = \ww (Z:.(_ ,b):._     ) -> ww `FM.snoc` ( fastChar fc "-"
                                                    , fastChar fc b
                                                    , fastDouble fd gapScore
                                                    )
  , indel = \ww (Z:._     :.(_ ,u)) -> ww `FM.snoc` ( fastChar fc u
                                                    , fastChar fc "-"
                                                    , fastDouble fd gapScore
                                                    )
  , align = \ww (Z:.(lb,b):.(lu,u)) -> let z = HM.lookupDefault defMismatch (Bigram lb b :!: Bigram lu u) bgm
                                       in  ww `FM.snoc` ( fastChar fc u
                                                        , fastChar fc b
                                                        , fastDouble fd z
                                                        )
  , done  = const $ FM.singleton ( fastChar fc "^"
                                 , fastChar fc "^"
                                 , fastChar fc "-"
                                 )
  , h     = SM.toList
  }
{-# Inline sBacktrackBuilder #-}

-- |

alignGlobalForward
  :: SimpleScoring
  -> Scores
  -> Vector IMCp
  -> Vector IMCp
  -> Z:.TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double
alignGlobalForward !simpleS !bgm !i1 !i2 = {-# SCC "ali_forw" #-} mutateTablesDefault $
  gGlobal (sScore simpleS bgm)
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []) )
    (chr i1) (chr i2)
  where !n1 = VU.length i1
        !n2 = VU.length i2
{-# NoInline alignGlobalForward #-}

-- |

alignGlobalBacktrackBuilder
  :: FastChars
  -> FastDoubles
  -> Int
  -> SimpleScoring
  -> Scores
  -> Vector IMCp
  -> Vector IMCp
  -> TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double
  -> [[B3]]
alignGlobalBacktrackBuilder !fc !fd !width !simpleS !bgm !i1 !i2 !t = {-# SCC "AliBtBuilder" #-} L.map FM.toList . unId $ axiom b
  where (Z:.b) = gGlobal (sScore simpleS bgm <|| sBacktrackBuilder fc fd width simpleS bgm) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline alignGlobalBacktrackBuilder #-}

-- |

alignGlobal
  :: SimpleScoring
  -> Scores
  -> FastChars
  -> FastDoubles
  -> Int
  -> Int
  -> Vector BTI
  -> Vector BTI
  -> (Double,[[B3]])
alignGlobal simpleS bgm fc fd width k i1' i2' = {-# SCC "alignGlobal" #-} (d, take k bs)
  where i1 = wordToBigramVector i1' ; i2 = wordToBigramVector i2'
        n1 = VU.length i1 ; n2 = VU.length i2
        !(Z:.t) = alignGlobalForward simpleS bgm i1 i2
        d = unId $ axiom t
        bs = alignGlobalBacktrackBuilder fc fd width simpleS bgm i1 i2 t
{-# NoInline alignGlobal #-}

