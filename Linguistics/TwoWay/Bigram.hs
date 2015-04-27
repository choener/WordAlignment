{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

module Linguistics.TwoWay.Bigram where

import           Data.ByteString.Char8 (ByteString)
import           Data.Strict.Tuple (Pair (..))
import           Data.Vector.Fusion.Util (Id(..))
import qualified Data.ByteString.Char8 as B
import qualified Data.HashTable.IO as H
import qualified Data.List as L
--import qualified Data.Vector as V
--import           Data.Vector (Vector)
import           Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import           Data.Sequence (Seq)
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Fusion.Stream as S
import           System.IO.Unsafe (unsafePerformIO)

import ADP.Fusion
import Data.PrimitiveArray
import NLP.Alphabet.MultiChar
import NLP.Alphabet.IMMC
import NLP.Scoring.SimpleUnigram
import DP.Alignment.Global.Tapes2

import Linguistics.Common

import           Linguistics.Bigram
import           Linguistics.Common
import           Linguistics.TwoWay.Common


type IMC = IMMC
type IMCp = (IMMC, IMMC)
type SigT m x r = SigGlobal m x r IMCp IMCp



sScore :: Monad m => Double -> Double -> Scores -> SigT m Double Double
sScore dS gapopen s = SigGlobal
  { delin = \ww (Z:.c     :._     ) -> ww + gapopen
  , indel = \ww (Z:._     :.c     ) -> ww + gapopen
  , align = \ww (Z:.(lp,l):.(up,u)) -> ww + lkup up u lp l
  , done = const 0
  , h         = SM.foldl' max (-888888)
  } where
    lkup mc' c nd' d = maybe dS id . unsafePerformIO $ H.lookup s (Bigram mc' c :!: Bigram nd' d)
    {-# INLINE lkup #-}
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

sPretty = prettyF ("-","-") ("-","-")
{-# Inline sPretty #-}

alignGlobal :: Double -> Double -> Scores -> Int -> Vector IMC -> Vector IMC -> (Double,[[(IMCp,IMCp)]])
alignGlobal ds gapopen scoring k i1' i2' = (d, take k . L.map runPrettyF . S.toList . unId $ axiom b) where
  i1 = VU.zip i1' (VU.tail i1') ; i2 = VU.zip i2' (VU.tail i2')
  n1 = VU.length i1 ; n2 = VU.length i2
  t :: ITbl Id Unboxed (Z:.PointL:.PointL) Double
  !(Z:.t) = mutateTablesDefault $ 
              gGlobal (sScore ds gapopen scoring)
                (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n2:.PointL n1) (-999999) []))
                (chr i1) (chr i2)
  d = unId $ axiom t
  !(Z:.b) = gGlobal (sScore ds gapopen scoring <** sPretty) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline alignGlobal #-}

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

