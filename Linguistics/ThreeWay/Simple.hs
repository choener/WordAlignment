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

sAlign :: Monad m => SThreeWay m (String,String) (S.Stream m (String,String)) ByteString ()
sAlign = SThreeWay
  { loop_loop_step = \(w1,w2,w3) (Z:.():.():.c ) -> (w1++padd "" c  , w2++padd "" c, w3++prnt c "")
  , loop_step_loop = \(w1,w2,w3) (Z:.():.b :.()) -> (w1++padd "" b  , w2++prnt b "", w3++padd "" b)
  , loop_step_step = \(w1,w2,w3) (Z:.():.b :.c ) -> (w1++pad2 "" b c, w2++prn2 b c , w3++prn2 c b )
  , step_loop_loop = \(w1,w2,w3) (Z:.a :.():.()) -> (w1++padd a ""  , 
  , step_loop_step = \(w1,w2,w3) (Z:.a :.():.c ) -> (w1++prnt a c   , w2++pad2 a c , w3++prnt c a )
  , step_step_loop = \(w1,w2,w3) (Z:.a :.b :.()) ->
  , step_step_step = \(w1,w2,w3) (Z:.a :.b :.c ) ->
  , nil_nil_nil    = const ("","","")
  , h              = return . id
  {-
  { loop_step = \(w1,w2) (Z:.():.c) -> (w1++padd "" c, w2++prnt c "")
  , step_loop = \(w1,w2) (Z:.c:.()) -> (w1++prnt c "", w2++padd "" c)
  , step_step = \(w1,w2) (Z:.a:.b) -> (w1++prnt a b,w2++prnt b a)
  , nil_nil   = const ("","")
  , h         = return . id
  -}
  } where prnt x z = printAligned x [z]
          padd x z = printAlignedPad '-' x [z]
{-# INLINE sAlign #-}

