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

module Linguistics.TwoWay where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import qualified Data.Vector.Fusion.Stream.Monadic as S hiding ((++))
import qualified Data.Vector.Fusion.Stream.Monadic as P hiding ((++))
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Data.Vector.Fusion.Util
import Data.Text (Text(..))
import qualified Data.Text as T
import Text.Printf
import qualified Data.Map as M
import Data.Strict.Tuple (Pair (..))
import Data.Char
import qualified Data.HashMap.Strict as H

import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import ADP.Fusion
import ADP.Fusion.None
import ADP.Fusion.Empty
import ADP.Fusion.Table
import ADP.Fusion.Chr
import ADP.Fusion.Multi

import Linguistics.Tools



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

sScore :: Monad m => Scores Text -> STwoWay m Double Double (Maybe Text,Text) ()
sScore  s = STwoWay
  { loop_step = \ww (Z:.():.(mc,c))     -> ww -4 -- in/del
  , step_loop = \ww (Z:.(mc,c):.())     -> ww -4 -- in/del
  , step_step = \ww (Z:.(mc,c):.(nd,d)) -> case (mc,nd) of
                                             (Nothing  , Nothing ) -> 0
                                             (Just mc' , Just nd') -> ww + H.lookupDefault (def s) (Bigram mc' c , Bigram nd' d) (scores s)
                                             _                     -> -500000
  , nil_nil   = const 0
  , h         = S.foldl' max (-500000)
  }
{-# INLINE sScore #-}

sAlign2 :: Monad m => STwoWay m (String,String) (S.Stream m (String,String)) (Maybe Text,Text) ()
sAlign2 = STwoWay
  { loop_step = \(w1,w2) (Z:.():.(_,c)) -> (w1++ds c  ,w2++prnt c "")
  , step_loop = \(w1,w2) (Z:.(_,c):.()) -> (w1++prnt c "",w2++ds c  )
  , step_step = \(w1,w2) (Z:.(_,a):.(_,b)) -> (w1++prnt a b,w2++prnt b a)
  , nil_nil   = const ("","")
  , h         = return . id
  } where prnt x z = let pad = max 0 (length (filter isAlphaNum $ T.unpack z) - length (filter isAlphaNum $ T.unpack x))
                     in  printf " %s%s" (replicate pad ' ') (T.unpack  x)
          ds   x = ' ' : replicate (length $ filter isAlphaNum $ T.unpack x) '-'

nWay2 scores i1 i2 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2), bt) where
  ws = unsafePerformIO (nWay2Fill scores i1 i2)
  n1 = V.length i1
  n2 = V.length i2
  bt = backtrack2 scores i1 i2 ws
{-# NOINLINE nWay2 #-}

nWay2Fill
  :: Scores Text
  -> V.Vector Text
  -> V.Vector Text
  -> IO (PA.Unboxed (Z:.PointL:.PointL) Double)
nWay2Fill scores i1 i2 = do
  let n1 = V.length i1
  let n2 = V.length i2
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2) 0
  let w = mTbl (Z:.EmptyT:.EmptyT) t'
  fillTable2 $ gTwoWay (sScore scores) w (chrLeft i1) (chrLeft i2) Empty Empty
  freeze t'
{-# INLINE nWay2Fill #-}

fillTable2 (Z:.(MTbl _ tbl, f)) = do
  let (_,Z:.PointL(0:.n1):.PointL(0:.n2)) = boundsM tbl
  forM_ [0 .. n1] $ \k1 -> forM_ [0 .. n2] $ \k2 -> do
    (f $ Z:.pointL 0 k1:.pointL 0 k2) >>= writeM tbl (Z:.pointL 0 k1:.pointL 0 k2)
{-# INLINE fillTable2 #-}

backtrack2 scores (i1 :: V.Vector Text) (i2 :: V.Vector Text) tbl = unId . P.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 where
  n1 = V.length i1
  n2 = V.length i2
  w :: DefBtTbl Id (Z:.PointL:.PointL) Double (String,String)
  w = btTbl (Z:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL) -> Id (S.Stream Id (String,String)))
  (Z:.(_,g)) = gTwoWay (sScore scores <** sAlign2) w (chrLeft i1) (chrLeft i2) Empty Empty

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

