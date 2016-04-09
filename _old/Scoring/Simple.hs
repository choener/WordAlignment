{-# LANGUAGE BangPatterns #-}

-- | The extracted part of the simple scoring scheme.

module Linguistics.Scoring.Simple where

import           Data.ByteString.Char8 (ByteString)
import           Data.Interned
import           Data.Interned.ByteString
import qualified Data.ByteString.Short as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector.Unboxed as VU

import           NLP.Alphabet.MultiChar



-- | Score the case of a match of two characters.

scoreMatch :: VU.Vector Char -> VU.Vector Char -> [Double] -> Double -> InternedMultiChar -> InternedMultiChar -> Double
scoreMatch vowels consonants scores gapOpen c d
  | c==d && cec = consonantIDS
  | c==d && cev = vowelIDS
  | cev && dev  = vowelS
  | cec && dec  = consonantS
  | cev && dec
  || cec && dev = vowelConsonantS
  | otherwise   = otherS
  where
    cev = T.any vowel     . conv $ c
    cec = T.any consonant . conv $ c
    dev = T.any vowel     . conv $ d
    dec = T.any consonant . conv $ d
    conv = T.toLower . T.decodeUtf8 . S.fromShort . unMultiChar . uninternMultiChar
    {-# INLINE conv #-}
    vowel     x = x `VU.elem` vowels
    {-# INLINE vowel #-}
    consonant x = x `VU.elem` consonants
    {-# INLINE consonant #-}
    [ !consonantIDS, !consonantS, !vowelIDS, !vowelS, !otherS, !vowelConsonantS ] = scores
{-# INLINE scoreMatch #-}

