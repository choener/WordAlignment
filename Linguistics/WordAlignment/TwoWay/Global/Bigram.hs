{- LANGUAGE MultiWayIf #-}
{- LANGUAGE OverloadedStrings #-}
{- LANGUAGE TypeOperators #-}
{- LANGUAGE BangPatterns #-}
{- LANGUAGE NoMonomorphismRestriction #-}

module Linguistics.WordAlignment.TwoWay.Global.Bigram where

import           Data.ByteString.Char8 (ByteString)
import           Data.FMList (FMList)
import qualified Data.FMList as FM
import           Data.Sequence (Seq)
import           Data.Strict.Tuple (Pair (..))
import           Data.Stringable (toString,toText)
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

import           ADP.Fusion
import           Data.PrimitiveArray
import           DP.Alignment.Global.Linear2
import           NLP.Scoring.SimpleUnigram
import           NLP.Text.BTI

import           Linguistics.WordAlignment.Common
import           Linguistics.WordAlignment.Bigram
import           Linguistics.WordAlignment.Word (FastChars, fastChar)



type IMC = BTI
type SigT m x r = SigGlobal m x r IMCp IMCp

-- |

sScore :: Monad m => Double -> Double -> Scores -> SigT m Double Double
sScore !dS !gapopen !s = SigGlobal
  { delin = \ww (Z:.c     :._     ) -> ww + gapopen
  , indel = \ww (Z:._     :.c     ) -> ww + gapopen
  , align = \ww (Z:.(lp,l):.(up,u)) -> ww + lkup up u lp l
  , done = const 0
  , h         = SM.foldl' max (-888888)
  } where
    lkup mc' c nd' d = {-# SCC "lkup" #-} HM.lookupDefault dS (Bigram mc' c :!: Bigram nd' d) s
    {-# Inline lkup #-}
{-# INLINE sScore #-}

-- |

sBacktrackBuilder :: Monad m => FastChars -> Int -> Double -> Double -> Scores -> SigT m (FMList B3) [FMList B3]
sBacktrackBuilder !fc !k !defS !go !sco = SigGlobal
  { delin = \ww (Z:.(_ ,b):._     ) -> ww `FM.snoc` ( fastChar fc "-"
                                                    , fastChar fc b
                                                    , TF.left k ' ' $ TF.fixed 1 go
                                                    )
  , indel = \ww (Z:._     :.(_ ,u)) -> ww `FM.snoc` ( fastChar fc u
                                                    , fastChar fc "-"
                                                    , TF.left k ' ' $ TF.fixed 1 go
                                                    )
  , align = \ww (Z:.(lb,b):.(lu,u)) -> let z = HM.lookupDefault defS (Bigram lb b :!: Bigram lu u) sco
                                       in  ww `FM.snoc` ( fastChar fc u
                                                        , fastChar fc b
                                                        , TF.left k ' ' $ TF.fixed 1 z
                                                        )
  , done  = const FM.empty
  , h     = SM.toList
  }
{-# Inline sBacktrackBuilder #-}

-- |

alignGlobalForward :: Double -> Double -> Scores -> Vector IMCp -> Vector IMCp -> Z:.TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double
alignGlobalForward !ds !gapopen !scoring !i1 !i2 = {-# SCC "ali_forw" #-} mutateTablesDefault $ -- {-# SCC "ali_forw/g" #-}
  gGlobal (sScore ds gapopen scoring)
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []) )
    (chr i1) (chr i2)
  where !n1 = VU.length i1
        !n2 = VU.length i2
{-# NoInline alignGlobalForward #-}

-- |

alignGlobalBacktrackBuilder :: FastChars -> Int -> Double -> Double -> Scores -> Vector IMCp -> Vector IMCp -> TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double -> [[B3]]
alignGlobalBacktrackBuilder !fc !k !ds !gapopen !scoring !i1 !i2 !t = {-# SCC "AliBtBuilder" #-} L.map runBacktrack . unId $ axiom b
  where (Z:.b) = gGlobal (sScore ds gapopen scoring <|| sBacktrackBuilder fc k ds gapopen scoring) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline alignGlobalBacktrackBuilder #-}

-- |

alignGlobal :: FastChars -> Int -> Double -> Double -> Scores -> Int -> Vector IMC -> Vector IMC -> (Double,[[B3]])
alignGlobal !fc !width !ds !gapopen !scoring !k !i1' !i2' = {-# SCC "aliGlob" #-} (d, take k bs) where -- . L.map runPrettyF . S.toList . unId $ axiom b) where
  i1 = mkI i1' ; i2 = mkI i2'
  n1 = VU.length i1 ; n2 = VU.length i2
  !(Z:.t) = alignGlobalForward ds gapopen scoring i1 i2
  d = unId $ axiom t
  bs = alignGlobalBacktrackBuilder fc width ds gapopen scoring i1 i2 t
  mkI m = {-# SCC "mkI" #-} VU.zip m (VU.tail m)
{-# NoInline alignGlobal #-}

