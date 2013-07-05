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
{-# OPTIONS_HADDOCK hide #-}

module Linguistics.FourWay.Backward where

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
import Linguistics.FourWay.Forward (sScore)



sAlign4 :: Monad m => SFourWay m (String,String,String,String) (S.Stream m (String,String,String,String)) (Maybe Char,Char) ()
sAlign4 = SFourWay
  { loop_loop_loop_step = alignfake
  , loop_loop_step_loop = alignfake
  , loop_loop_step_step = alignfake
  , loop_step_loop_loop = alignfake
  , loop_step_loop_step = alignfake
  , loop_step_step_loop = alignfake
  , loop_step_step_step = alignfake
  , step_loop_loop_loop = alignfake
  , step_loop_loop_step = alignfake
  , step_loop_step_loop = alignfake
  , step_loop_step_step = alignfake
  , step_step_loop_loop = alignfake
  , step_step_loop_step = alignfake
  , step_step_step_loop = alignfake
  , step_step_step_step = alignfake
  , nil_nil_nil_nil = const ("","","","")
  , h         = return . id
  }
{-# INLINE sAlign4 #-}

alignfake (w1,w2,w3,w4) (Z:.a:.b:.c:.d) = (w1++addAlign a, w2++addAlign b, w3++addAlign c, w4++addAlign d)

class AddAlign x where
  addAlign :: x -> String

instance AddAlign () where
  addAlign () = "-"

instance AddAlign PC where
  addAlign (_,a) = [a]

backtrack4 (i1 :: VU.Vector Char) (i2 :: VU.Vector Char) (i3 :: VU.Vector Char) (i4 :: VU.Vector Char) tbl
  = unId . P.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 :.pointL 0 n3 :.pointL 0 n4
  where
    n1 = VU.length i1
    n2 = VU.length i2
    n3 = VU.length i3
    n4 = VU.length i4
    w :: DefBtTbl Id (Z:.PointL:.PointL:.PointL:.PointL) Int (String,String,String,String)
    w = btTbl (Z:.EmptyT:.EmptyT:.EmptyT:.EmptyT) tbl g -- (g :: (Z:.PointL:.PointL:.PointL:.PointL) -> Id (S.Stream Id (String,String,String,String)))
    (Z:.(_,g)) = gFourWay (sScore <** sAlign4) w (chrLeft i1) (chrLeft i2) (chrLeft i3) (chrLeft i4) Empty Empty Empty Empty
{-# NOINLINE backtrack4 #-}

