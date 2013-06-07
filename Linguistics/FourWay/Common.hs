{-# LANGUAGE TypeOperators #-}

module Linguistics.FourWay.Common where

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
  , h :: S.Stream _m _x -> _m _r
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

type PC = (Maybe Char,Char)

