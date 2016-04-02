{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

module Linguistics.TwoWay.Bigram where

import           Data.ByteString.Char8 (ByteString)
import           Data.FMList (FMList)
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

import           ADP.Fusion
import           Data.PrimitiveArray
import           DP.Alignment.Global.Tapes2
import           NLP.Scoring.SimpleUnigram
import           NLP.Text.BTI

import           Linguistics.Common
import           Linguistics.Bigram



type IMC = BTI
type SigT m x r = SigGlobal m x r IMCp IMCp



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
{-
sScore dS gapOpen s = SigGlobal
  { indel = \ww (Z:.():.(mc,c))     -> if | c=="$"    -> -500000
                                          | otherwise -> ww + gapOpen
  , delin = \ww (Z:.(mc,c):.())     -> if | c=="$"    -> -500000
                                          | otherwise -> ww + gapOpen
  , align = \ww (Z:.(mc,c):.(nd,d)) -> if | c=="^" && d=="$" -> -500000
                                          | c=="$" && d=="^" -> -500000
                                              | otherwise        -> case (mc,nd) of
                                                  (Nothing  , Nothing ) -> 0
                                                  (Just mc' , Just nd') -> ww + lkup mc' c nd' d
                                                  _                     -> -500000
  , done   = const 0
  , h         = S.foldl' max (-500000)
  } where
    lkup mc' c nd' d = maybe dS id . unsafePerformIO $ H.lookup s (Bigram mc' c :!: Bigram nd' d)
    {-# INLINE lkup #-}
{-# INLINE sScore #-}
-}

sBacktrack :: Monad m => SigT m (FMList (IMCp,IMCp)) [FMList (IMCp,IMCp)]
sBacktrack = backtrack ("-","-") ("-","-")
{-# Inline sBacktrack #-}

sBacktrackFun :: Monad m => Double -> Double -> Scores -> SigT m (FMList [Text]) [FMList [Text]]
sBacktrackFun defS go sco = backtrackFun f g ("-","-") ("-","-") where
  f cc@(mc',c) dd@(nd',d) = let z = HM.lookupDefault defS (Bigram mc' c :!: Bigram nd' d) sco
    in [toText c,toText d, pack $ printf "%3.1f" z]
  g    (_  ,c)    ( _  ,"-") = [toText c,"-", pack $ printf "%3.1f" go]
  g    (_, "-") (_,d) = ["-", toText d, pack $ printf "%3.1f" go]
{-# Inline sBacktrackFun #-}

alignGlobal :: Double -> Double -> Scores -> Int -> Vector IMC -> Vector IMC -> (Double,[[[Text]]])
alignGlobal !ds !gapopen !scoring !k !i1' !i2' = {-# SCC "aliGlob" #-} (d, take k bs) where -- . L.map runPrettyF . S.toList . unId $ axiom b) where
  i1 = mkI i1' ; i2 = mkI i2'
  n1 = VU.length i1 ; n2 = VU.length i2
  !(Z:.t) = alignGlobalForward ds gapopen scoring i1 i2
  d = unId $ axiom t
  bs = alignGlobalBacktrack ds gapopen scoring i1 i2 t
  mkI m = {-# SCC "mkI" #-} VU.zip m (VU.tail m)
{-# NoInline alignGlobal #-}

alignGlobalForward :: Double -> Double -> Scores -> Vector IMCp -> Vector IMCp -> Z:.TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double
alignGlobalForward !ds !gapopen !scoring !i1 !i2 = {-# SCC "ali_forw" #-} mutateTablesDefault $ {-# SCC "ali_forw/g" #-}
  gGlobal (sScore ds gapopen scoring)
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []) )
    (chr i1) (chr i2)
  where !n1 = VU.length i1
        !n2 = VU.length i2
{-# NoInline alignGlobalForward #-}

alignGlobalBacktrack :: Double -> Double -> Scores -> Vector IMCp -> Vector IMCp -> TwITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Double -> [[[Text]]]
alignGlobalBacktrack ds gapopen scoring i1 i2 t = {-# SCC "ali_back" #-} L.map runBacktrack . unId $ axiom b
  where (Z:.b) = gGlobal (sScore ds gapopen scoring <|| sBacktrackFun ds gapopen scoring) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline alignGlobalBacktrack #-}

{-
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

-}

