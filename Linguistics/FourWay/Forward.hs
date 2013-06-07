{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
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

module Linguistics.FourWay.Forward where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Fusion.Stream.Monadic as P hiding ((++))
import qualified Data.Vector.Unboxed as VU
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Data.Vector.Fusion.Util
import TupleTH

import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import ADP.Fusion
import ADP.Fusion.None
import ADP.Fusion.Empty
import ADP.Fusion.Table
import ADP.Fusion.Chr
import ADP.Fusion.Multi

import Linguistics.FourWay.Common



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

loopstep w (Z:.a:.b:.c:.d) = w + (sum [pairScore a b, pairScore a c, pairScore a d, pairScore b c, pairScore b d, pairScore c d])
{-# INLINE loopstep #-}

class PairScore l r where
  pairScore :: l -> r -> Int

instance PairScore PC PC where
  pairScore (ma,a) (mb,b) = if a==b then 1 else 0

instance PairScore PC () where
  pairScore _ _ = 0

instance PairScore () PC where
  pairScore _ _ = 0

instance PairScore () () where
  pairScore _ _ = 0

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
{-# NOINLINE nWay4Fill #-}

fillTable4 (Z:.(MTbl _ tbl, f)) = do
  let (_,Z:.PointL(0:.n1):.PointL(0:.n2):.PointL(0:.n3):.PointL(0:.n4)) = boundsM tbl
  forM_ [0 .. n1] $ \k1 -> forM_ [0 .. n2] $ \k2 -> forM_ [0 .. n3] $ \k3 -> forM_ [0 .. n4] $ \k4 -> do
    (f $ Z:.pointL 0 k1:.pointL 0 k2:.pointL 0 k3:.pointL 0 k4) >>= writeM tbl (Z:.pointL 0 k1:.pointL 0 k2:.pointL 0 k3:.pointL 0 k4)
{-# INLINE fillTable4 #-}

