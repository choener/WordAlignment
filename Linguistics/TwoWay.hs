{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-liberate-case #-}

-- |
--
-- NOTE currently does ~ 4300 alignments (with backtracking) / second.

module Linguistics.TwoWay where

import qualified Data.List as L
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import qualified Data.Vector.Fusion.Stream.Monadic as S hiding ((++))
import qualified Data.Vector.Fusion.Stream.Monadic as P hiding ((++))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Data.Vector.Fusion.Util
import Text.Printf
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.Strict.Tuple (Pair (..))
import Data.Char hiding (chr)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.HashTable.IO as H

import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import ADP.Fusion
import ADP.Fusion.None
import ADP.Fusion.Empty
import ADP.Fusion.Table
import ADP.Fusion.Chr
import ADP.Fusion.Multi

import Linguistics.Bigram
import Linguistics.Common

import Debug.Trace



data STwoWay _m _x _r c empty = STwoWay
  { loop_step :: _x -> (Z:.():.c) -> _x
  , step_loop :: _x -> (Z:.c:.()) -> _x
  , step_step :: _x -> (Z:.c:.c)  -> _x
  , nil_nil :: (Z:.empty:.empty)  -> _x
  , h :: S.Stream _m _x -> _m _r
  }

gTwoWay sTwoWay {-non-terminals:-} ww {-terminals:-} c1 c2 empty1 empty2 =
  (Z:.
  ( ww ,  loop_step sTwoWay  <<< ww % (T:!None:!c2)  |||
          step_loop sTwoWay  <<< ww % (T:!c1:!None)  |||
          step_step sTwoWay  <<< ww % (T:!c1:!c2)    |||
          nil_nil   sTwoWay  <<< (T:!empty1:!empty2) ... h sTwoWay )
  )
{-# INLINE gTwoWay #-}

sScore :: Monad m => Double -> Double -> Scores -> STwoWay m Double Double (Maybe ByteString,ByteString) ()
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

sScoreSimple :: Monad m => [Double] -> Double -> STwoWay m Double Double ByteString ()
sScoreSimple scores gapOpen = STwoWay
  { loop_step = \ww (Z:.():.c)     -> ww + gapOpen
  , step_loop = \ww (Z:.c:.())     -> ww + gapOpen
  , step_step = \ww (Z:.c:.d ) -> let cev = any (`elem` vowel)     $ B.unpack {- toUtf8String -} c
                                      cec = any (`elem` consonant) $ B.unpack {- toUtf8String -} c
                                      dev = any (`elem` vowel)     $ B.unpack {- toUtf8String -} d
                                      dec = any (`elem` consonant) $ B.unpack {- toUtf8String -} d
                                  in ww + if
                  | c==d && cec -> consonantIDS
                  | c==d && cev -> vowelIDS
                  | cev && dev  -> vowelS
                  | cec && dec  -> consonantS
                  | cev && dec || cec && dev -> vowelConsonantS
                  | otherwise   -> otherS
  , nil_nil   = const 0
  , h         = S.foldl' max (-500000)
  } where
    vowel = "aeiou"
    consonant = ['a' .. 'z'] L.\\ vowel
    [consonantIDS,consonantS,vowelIDS,vowelS,otherS,vowelConsonantS] = scores
{-# INLINE sScoreSimple #-}

-- | Backtrack the alignment

sAlign2 :: Monad m => STwoWay m (String,String) (S.Stream m (String,String)) (Maybe ByteString,ByteString) ()
sAlign2 = STwoWay
  { loop_step = \(w1,w2) (Z:.():.(_,c)) -> (w1++padd "" c, w2++prnt c "")
  , step_loop = \(w1,w2) (Z:.(_,c):.()) -> (w1++prnt c "", w2++padd "" c)
  , step_step = \(w1,w2) (Z:.(_,a):.(_,b)) -> (w1++prnt a b,w2++prnt b a)
  , nil_nil   = const ("","")
  , h         = return . id
  } where prnt x z = printAligned x [z]
          padd x z = printAlignedPad '-' x [z]

sAlign2Simple :: Monad m => STwoWay m (String,String) (S.Stream m (String,String)) ByteString ()
sAlign2Simple = STwoWay
  { loop_step = \(w1,w2) (Z:.():.c) -> (w1++padd "" c, w2++prnt c "")
  , step_loop = \(w1,w2) (Z:.c:.()) -> (w1++prnt c "", w2++padd "" c)
  , step_step = \(w1,w2) (Z:.a:.b) -> (w1++prnt a b,w2++prnt b a)
  , nil_nil   = const ("","")
  , h         = return . id
  } where prnt x z = printAligned x [z]
          padd x z = printAlignedPad '-' x [z]

-- | Wrap calculations

nWay2 dS gapOpen scores i1 i2 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2), bt) where
  ws = unsafePerformIO (nWay2Fill dS gapOpen scores i1 i2)
  n1 = V.length i1
  n2 = V.length i2
  bt = backtrack2 dS gapOpen scores i1 i2 ws
{-# NOINLINE nWay2 #-}

nWay2Simple scores gapOpen i1 i2 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2), bt) where
  ws = unsafePerformIO (nWay2FillSimple scores gapOpen i1 i2)
  n1 = V.length i1
  n2 = V.length i2
  bt = backtrack2Simple scores gapOpen i1 i2 ws
{-# NOINLINE nWay2Simple #-}

-- | Forward phase

nWay2Fill
  :: Double
  -> Double
  -> Scores
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> IO (PA.Unboxed (Z:.PointL:.PointL) Double)
nWay2Fill dS gapOpen scores i1 i2 = do
  let n1 = V.length i1
  let n2 = V.length i2
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2) 0
  let w = mTbl (Z:.EmptyT:.EmptyT) t'
  fillTable2 $ gTwoWay (sScore dS gapOpen scores) w (chrLeft i1) (chrLeft i2) Empty Empty
  freeze t'
{-# INLINE nWay2Fill #-}

nWay2FillSimple
  :: [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> IO (PA.Unboxed (Z:.PointL:.PointL) Double)
nWay2FillSimple scores gapOpen i1 i2 = do
  let n1 = V.length i1
  let n2 = V.length i2
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2) 0
  let w = mTbl (Z:.EmptyT:.EmptyT) t'
  fillTable2 $ gTwoWay (sScoreSimple scores gapOpen) w (chr i1) (chr i2) Empty Empty
  freeze t'
{-# INLINE nWay2FillSimple #-}

-- | Fill 2-dim table

fillTable2 (Z:.(MTbl _ tbl, f)) = do
  let (_,Z:.PointL(0:.n1):.PointL(0:.n2)) = boundsM tbl
  --forM_ [1 .. n1] $ \k1 -> forM_ [1 .. n2] $ \k2 -> do
  forM_ [0 .. n1] $ \k1 -> forM_ [0 .. n2] $ \k2 -> do
    (f $ Z:.pointL 0 k1:.pointL 0 k2) >>= writeM tbl (Z:.pointL 0 k1:.pointL 0 k2)
{-# INLINE fillTable2 #-}

-- | Backtrack results

backtrack2
  :: Double
  -> Double
  -> Scores
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> PA.Unboxed (Z:.PointL:.PointL) Double
  -> [(String,String)]
backtrack2 dS gapOpen scores i1 i2 tbl = unId . P.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 where
  n1 = V.length i1
  n2 = V.length i2
  w :: DefBtTbl Id (Z:.PointL:.PointL) Double (String,String)
  w = btTbl (Z:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL) -> Id (S.Stream Id (String,String)))
  (Z:.(_,g)) = gTwoWay (sScore dS gapOpen scores <** sAlign2) w (chrLeft i1) (chrLeft i2) Empty Empty

backtrack2Simple
  :: [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> PA.Unboxed (Z:.PointL:.PointL) Double
  -> [(String,String)]
backtrack2Simple scores gapOpen i1 i2 tbl = unId . P.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 where
  n1 = V.length i1
  n2 = V.length i2
  w :: DefBtTbl Id (Z:.PointL:.PointL) Double (String,String)
  w = btTbl (Z:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL) -> Id (S.Stream Id (String,String)))
  (Z:.(_,g)) = gTwoWay (sScoreSimple scores gapOpen <** sAlign2Simple) w (chr i1) (chr i2) Empty Empty

-- | Algebra product operation

(<**) f s = STwoWay l_s s_l s_s n_n h where
  STwoWay lsf slf ssf nnf hf = f -- (emptyF,leftF,rightF,pairF,splitF,hF) = f
  STwoWay lss sls sss nns hs = s -- (emptyS,leftS,rightS,pairS,splitS,hS) = s
  l_s = go lsf lss
  s_l = go slf sls
  s_s = go ssf sss
  n_n e = (nnf e, return $ S.singleton $ nns e)
  h xs = do
    hfs <- hf $ S.map fst xs
    let phfs = S.concatMapM snd . S.filter ((hfs==) . fst) $ xs
    hs phfs
  go funL funR (x,ys) c = (funL x c, ys >>= return . S.map (\y -> funR y c))



test (s :: String) = nWay2Simple [3,1,1,0,0,-1] (-1) s' s' where
  s' = V.fromList $ L.map (B.pack . (:[])) $ s
