{-# LANGUAGE BangPatterns #-}

module Linguistics.ThreeWay.Simple where

import Data.Array.Repa.Index
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Fusion.Stream.Monadic as S
import Data.ByteString.Char8 (ByteString)

import Linguistics.Scoring.Simple
import Linguistics.ThreeWay.Common



sScore :: Monad m => VU.Vector Char -> VU.Vector Char -> [Double] -> Double -> SThreeWay m Double Double ByteString ()
sScore !vowels !consonants !scores !gapOpen = SThreeWay
  { loop_loop_step = \ww (Z:.():.():.c ) -> ww + gapOpen + gapOpen + 0 -- 0 is the mnemonic for loop/loop match
  , loop_step_loop = \ww (Z:.():.b :.()) -> ww + gapOpen + gapOpen + 0
  , loop_step_step = \ww (Z:.():.b :.c ) -> ww + gapOpen + gapOpen + sm b c
  , step_loop_loop = \ww (Z:.a :.():.()) -> ww + gapOpen + gapOpen + 0
  , step_loop_step = \ww (Z:.a :.():.c ) -> ww + gapOpen + gapOpen + sm a c
  , step_step_loop = \ww (Z:.a :.b :.()) -> ww + gapOpen + gapOpen + sm a b
  , step_step_step = \ww (Z:.a :.b :.c ) -> ww + sm a b + sm a c + sm b c
  , nil_nil_nil    = const 0
  , h              = S.foldl' max (-500000)
  } where sm = scoreMatch vowels consonants scores gapOpen
          {-# INLINE sm #-}
{-# INLINE sScore #-}

