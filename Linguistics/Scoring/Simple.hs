{-# LANGUAGE BangPatterns #-}

-- | The extracted part of the simple scoring scheme.

module Linguistics.Scoring.Simple where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Unboxed as VU
import Data.ByteString.Char8 (ByteString)



-- | Score the case of a match of two characters.

scoreMatch :: VU.Vector Char -> VU.Vector Char -> [Double] -> Double -> ByteString -> ByteString -> Double
scoreMatch vowels consonants scores gapOpen c d
  | c==d && cec = consonantIDS
  | c==d && cev = vowelIDS
  | cev && dev  = vowelS
  | cec && dec  = consonantS
  | cev && dec
  || cec && dev = vowelConsonantS
  | otherwise   = otherS
  where
    cev = T.any vowel     . T.toLower . T.decodeUtf8 $ c
    cec = T.any consonant . T.toLower . T.decodeUtf8 $ c
    dev = T.any vowel     . T.toLower . T.decodeUtf8 $ d
    dec = T.any consonant . T.toLower . T.decodeUtf8 $ d
    vowel     x = x `VU.elem` vowels
    {-# INLINE vowel #-}
    consonant x = x `VU.elem` consonants
    {-# INLINE consonant #-}
    [ !consonantIDS, !consonantS, !vowelIDS, !vowelS, !otherS, !vowelConsonantS ] = scores
{-# INLINE scoreMatch #-}

