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
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Data.Vector.Fusion.Util

import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import ADP.Fusion
import ADP.Fusion.None
import ADP.Fusion.Empty
import ADP.Fusion.Table
import ADP.Fusion.Chr
import ADP.Fusion.Multi



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

sTwoWay :: Monad m => STwoWay m Int Int (Maybe Char,Char) ()
sTwoWay = STwoWay
  { loop_step = \ww (Z:.():.(mc,c))     -> ww -- in/del
  , step_loop = \ww (Z:.(mc,c):.())     -> ww -- in/del
  , step_step = \ww (Z:.(mc,c):.(nd,d)) -> ww + if c==d then 1 else 0 -- align
  , nil_nil   = const 0
  , h         = S.foldl' max 0
  }
{-# INLINE sTwoWay #-}

sAlign2 :: Monad m => STwoWay m (String,String) (S.Stream m (String,String)) (Maybe Char,Char) ()
sAlign2 = STwoWay
  { loop_step = \(w1,w2) (Z:.():.(_,c)) -> (w1++"-",w2++[c])
  , step_loop = \(w1,w2) (Z:.(_,c):.()) -> (w1++[c],w2++"-")
  , step_step = \(w1,w2) (Z:.(_,a):.(_,b)) -> (w1++[a],w2++[b])
  , nil_nil   = const ("","")
  , h         = return . id
  }

nWay2 i1' i2' = (ws ! (Z:.pointL 0 n1:.pointL 0 n2), bt) where
  i1 = VU.fromList i1'
  i2 = VU.fromList i2'
  ws = unsafePerformIO (nWay2Fill i1 i2)
  n1 = VU.length i1
  n2 = VU.length i2
  bt = backtrack2 i1 i2 ws
{-# NOINLINE nWay2 #-}

nWay2Fill
  :: VU.Vector Char
  -> VU.Vector Char
  -> IO (PA.Unboxed (Z:.PointL:.PointL) Int)
nWay2Fill i1 i2 = do
  let n1 = VU.length i1
  let n2 = VU.length i2
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2) 0
  let w = mTbl (Z:.EmptyT:.EmptyT) t'
  fillTable2 $ gTwoWay sTwoWay w (chrLeft i1) (chrLeft i2) Empty Empty
  freeze t'
{-# INLINE nWay2Fill #-}

fillTable2 (Z:.(MTbl _ tbl, f)) = do
  let (_,Z:.PointL(0:.n1):.PointL(0:.n2)) = boundsM tbl
  forM_ [0 .. n1] $ \k1 -> forM_ [0 .. n2] $ \k2 -> do
    (f $ Z:.pointL 0 k1:.pointL 0 k2) >>= writeM tbl (Z:.pointL 0 k1:.pointL 0 k2)
{-# INLINE fillTable2 #-}

backtrack2 (i1 :: VU.Vector Char) (i2 :: VU.Vector Char) tbl = unId . P.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 where
  n1 = VU.length i1
  n2 = VU.length i2
  w :: DefBtTbl Id (Z:.PointL:.PointL) Int (String,String)
  w = btTbl (Z:.EmptyT:.EmptyT) tbl (g :: (Z:.PointL:.PointL) -> Id (S.Stream Id (String,String)))
  (Z:.(_,g)) = gTwoWay (sTwoWay <** sAlign2) w (chrLeft i1) (chrLeft i2) Empty Empty

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

