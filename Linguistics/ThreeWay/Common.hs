{-# LANGUAGE TypeOperators #-}

module Linguistics.ThreeWay.Common where

import Data.Array.Repa.Index
import Data.Vector.Fusion.Stream.Monadic (Stream (..))

import ADP.Fusion
import ADP.Fusion.Multi



data SThreeWay _m _x _r c empty = SThreeWay
  { loop_loop_step :: _x -> (((Z:.()):.()):.c) -> _x
  , loop_step_loop :: _x -> (((Z:.()):.c):.()) -> _x
  , loop_step_step :: _x -> (((Z:.()):.c):.c) -> _x
  , step_loop_loop :: _x -> (((Z:.c):.()):.()) -> _x
  , step_loop_step :: _x -> (((Z:.c):.()):.c) -> _x
  , step_step_loop :: _x -> (((Z:.c):.c):.()) -> _x
  , step_step_step :: _x -> (((Z:.c):.c):.c) -> _x
  , nil_nil_nil :: (((Z:.empty):.empty):.empty) -> _x
  , h :: Stream _m _x -> _m _r
  }

gThreeWay sThreeWay {-non-terminals:-} www {-terminals:-} c_1 c_2 c_3 empty_1 empty_2 empty_3 =
  (Z:.
  ( www , loop_loop_step sThreeWay <<< www % (T:!None:!None:!c_3)     |||
          loop_step_loop sThreeWay <<< www % (T:!None:!c_2:!None)     |||
          loop_step_step sThreeWay <<< www % (T:!None:!c_2:!c_3)      |||
          step_loop_loop sThreeWay <<< www % (T:!c_1:!None:!None)     |||
          step_loop_step sThreeWay <<< www % (T:!c_1:!None:!c_3)      |||
          step_step_loop sThreeWay <<< www % (T:!c_1:!c_2:!None)      |||
          step_step_step sThreeWay <<< www % (T:!c_1:!c_2:!c_3)       |||
          nil_nil_nil    sThreeWay <<< (T:!empty_1:!empty_2:!empty_3) ... h sThreeWay )
  )
{-# INLINE gThreeWay #-}

