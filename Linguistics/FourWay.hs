{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE TemplateHaskell #-}
{- LANGUAGE ScopedTypeVariables #-}
{- LANGUAGE RankNTypes #-}
{- LANGUAGE FlexibleContexts #-}
{- LANGUAGE BangPatterns #-}
{- LANGUAGE FlexibleInstances #-}
{- LANGUAGE TypeOperators #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE TypeFamilies #-}
{- LANGUAGE RecordWildCards #-}

module Linguistics.FourWay
  ( fourWaySimple
  , fourWayBigram
  ) where

import qualified Linguistics.FourWay.Simple as Simple
import qualified Linguistics.FourWay.Bigram as Bigram

fourWaySimple = Simple.fourWay
fourWayBigram = Bigram.fourWay

{-
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
import Linguistics.FourWay.Forward
import Linguistics.FourWay.Backward



nWay4 i1 i2 i3 i4 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3:.pointL 0 n4), bt) where
  ws = unsafePerformIO (nWay4Fill i1 i2 i3 i4)
  n1 = VU.length i1
  n2 = VU.length i2
  n3 = VU.length i3
  n4 = VU.length i4
  bt = backtrack4 i1 i2 i3 i4 ws
{-# NOINLINE nWay4 #-}
-}

