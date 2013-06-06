{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Linguistics.FourWay where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Unboxed as VU
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad

import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import ADP.Fusion
import ADP.Fusion.None
import ADP.Fusion.Empty
import ADP.Fusion.Table
import ADP.Fusion.Chr
import ADP.Fusion.Multi



data SFourWay _m _x _r c empty = SFourWay
  { loop_loop_loop_step :: _x -> ((((Z:.()):.()):.()):.c) -> _x
  , loop_loop_step_loop :: _x -> ((((Z:.()):.()):.c):.()) -> _x
  , loop_loop_step_step :: _x -> ((((Z:.()):.()):.c):.c) -> _x
  , loop_step_loop_loop :: _x -> ((((Z:.()):.c):.()):.()) -> _x
  , loop_step_loop_step :: _x -> ((((Z:.()):.c):.()):.c) -> _x
  , loop_step_step_loop :: _x -> ((((Z:.()):.c):.c):.()) -> _x
  , loop_step_step_step :: _x -> ((((Z:.()):.c):.c):.c) -> _x
  , step_loop_loop_loop :: _x -> ((((Z:.c):.()):.()):.()) -> _x
  , step_loop_loop_step :: _x -> ((((Z:.c):.()):.()):.c) -> _x
  , step_loop_step_loop :: _x -> ((((Z:.c):.()):.c):.()) -> _x
  , step_loop_step_step :: _x -> ((((Z:.c):.()):.c):.c) -> _x
  , step_step_loop_loop :: _x -> ((((Z:.c):.c):.()):.()) -> _x
  , step_step_loop_step :: _x -> ((((Z:.c):.c):.()):.c) -> _x
  , step_step_step_loop :: _x -> ((((Z:.c):.c):.c):.()) -> _x
  , step_step_step_step :: _x -> ((((Z:.c):.c):.c):.c) -> _x
  , nil_nil_nil_nil :: ((((Z:.empty):.empty):.empty):.empty) -> _x
  , h :: Stream _m _x -> _m _r
  }

gFourWay sFourWay {-non-terminals:-} wwww {-terminals:-} c_1 c_2 c_3 c_4 empty_1 empty_2 empty_3 empty_4 =
  (Z:.
  ( wwww ,  loop_loop_loop_step sFourWay <<< wwww % (T:!None:!None:!None:!c_4) |||
            loop_loop_step_loop sFourWay <<< wwww % (T:!None:!None:!c_3:!None) |||
            loop_loop_step_step sFourWay <<< wwww % (T:!None:!None:!c_3:!c_4) |||
            loop_step_loop_loop sFourWay <<< wwww % (T:!None:!c_2:!None:!None) |||
            loop_step_loop_step sFourWay <<< wwww % (T:!None:!c_2:!None:!c_4) |||
            loop_step_step_loop sFourWay <<< wwww % (T:!None:!c_2:!c_3:!None) |||
            loop_step_step_step sFourWay <<< wwww % (T:!None:!c_2:!c_3:!c_4) |||
            step_loop_loop_loop sFourWay <<< wwww % (T:!c_1:!None:!None:!None) |||
            step_loop_loop_step sFourWay <<< wwww % (T:!c_1:!None:!None:!c_4) |||
            step_loop_step_loop sFourWay <<< wwww % (T:!c_1:!None:!c_3:!None) |||
            step_loop_step_step sFourWay <<< wwww % (T:!c_1:!None:!c_3:!c_4) ||| 
            step_step_loop_loop sFourWay <<< wwww % (T:!c_1:!c_2:!None:!None) |||
            step_step_loop_step sFourWay <<< wwww % (T:!c_1:!c_2:!None:!c_4) ||| 
            step_step_step_loop sFourWay <<< wwww % (T:!c_1:!c_2:!c_3:!None) |||
            step_step_step_step sFourWay <<< wwww % (T:!c_1:!c_2:!c_3:!c_4) |||
            nil_nil_nil_nil sFourWay <<< (T:!empty_1:!empty_2:!empty_3:!empty_4) ... h sFourWay )
  )
{-# INLINE gFourWay #-}

sScore :: Monad m => SFourWay m Int Int (Maybe Char,Char) ()
sScore = SFourWay
  { loop_loop_loop_step = loopstep
  , loop_loop_step_loop = loopstep
  , loop_loop_step_step = loopstep
  , loop_step_loop_loop = loopstep
  , loop_step_loop_step = loopstep
  , loop_step_step_loop = loopstep
  , loop_step_step_step = loopstep
  , step_loop_loop_loop = loopstep
  , step_loop_loop_step = loopstep
  , step_loop_step_loop = loopstep
  , step_loop_step_step = loopstep
  , step_step_loop_loop = loopstep
  , step_step_loop_step = loopstep
  , step_step_step_loop = loopstep
  , step_step_step_step = loopstep
  , nil_nil_nil_nil = const 0
  , h         = S.foldl' max 0
  }
{-# INLINE sScore #-}

loopstep a b = 0

{-
sAlign2 :: Monad m => STwoWay m (String,String) (S.Stream m (String,String)) (Maybe Char,Char) ()
sAlign2 = STwoWay
  { loop_step = \(w1,w2) (Z:.():.(_,c)) -> (w1++"-",w2++[c])
  , step_loop = \(w1,w2) (Z:.(_,c):.()) -> (w1++[c],w2++"-")
  , step_step = \(w1,w2) (Z:.(_,a):.(_,b)) -> (w1++[a],w2++[b])
  , nil_nil   = const ("","")
  , h         = return . id
  }
-}

nWay4 i1 i2 i3 i4 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3:.pointL 0 n4), bt) where
  ws = unsafePerformIO (nWay4Fill i1 i2 i3 i4)
  n1 = VU.length i1
  n2 = VU.length i2
  n3 = VU.length i3
  n4 = VU.length i4
  bt = [] -- backtrack4 i1 i2 i3 i4 ws
{-# NOINLINE nWay4 #-}

nWay4Fill
  :: VU.Vector Char
  -> VU.Vector Char
  -> VU.Vector Char
  -> VU.Vector Char
  -> IO (PA.Unboxed (Z:.PointL:.PointL:.PointL:.PointL) Int)
nWay4Fill i1 i2 i3 i4 = do
  let n1 = VU.length i1
  let n2 = VU.length i2
  let n3 = VU.length i3
  let n4 = VU.length i4
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3:.pointL 0 n4) 0
  let w = mTbl (Z:.EmptyT:.EmptyT:.EmptyT:.EmptyT) t'
  fillTable4 $ gFourWay sScore w (chrLeft i1) (chrLeft i2) (chrLeft i3) (chrLeft i4) Empty Empty Empty Empty
  freeze t'
{-# INLINE nWay4Fill #-}

fillTable4 (Z:.(MTbl _ tbl, f)) = do
  let (_,Z:.PointL(0:.n1):.PointL(0:.n2):.PointL(0:.n3):.PointL(0:.n4)) = boundsM tbl
  forM_ [0 .. n1] $ \k1 -> forM_ [0 .. n2] $ \k2 -> forM_ [0 .. n3] $ \k3 -> forM_ [0 .. n4] $ \k4 -> do
    (f $ Z:.pointL 0 k1:.pointL 0 k2:.pointL 0 k3:.pointL 0 k4) >>= writeM tbl (Z:.pointL 0 k1:.pointL 0 k2:.pointL 0 k3:.pointL 0 k4)
{-# INLINE fillTable4 #-}

{-
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
-}
