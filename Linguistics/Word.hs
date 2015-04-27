{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | A single word in a language. Uses a 'MultiChar' encoding for the actual
-- characters. MultiChar encodings need to be decoded for printing on screen.

module Linguistics.Word where

import           Control.Applicative
import           Control.DeepSeq
import           Data.ByteString (ByteString)
import           Data.Interned
import           Data.Interned.ByteString
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AB hiding (takeWhile1)
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL hiding (readFile)
import qualified Data.ByteString.Short as S
import qualified Data.Vector.Unboxed as VU
import           Prelude hiding (Word)
import           Data.Stringable

import NLP.Alphabet.MultiChar
import NLP.Alphabet.IMMC


-- | A single word we want to align to another word. It comes with an id (here
-- 9), the language name (which we intern), a word class (interned as well),
-- the length of the word (so that we don't have to check wordWord length and
-- check for word delims), and finally the word itself. Indivitual 'MultiChar'
-- characters are interned to reduce memory cost (and we might want to do stuff
-- with the Id's).
--
--  9 Albanian_Tosk 1.214 6 \' b a lʸ t ə
--

data Word = Word
  { wordID     :: {-# UNPACK #-} !Int
  , wordClass  :: {-# UNPACK #-} !IMMC -- InternedByteString
  , wordLang   :: {-# UNPACK #-} !IMMC -- InternedByteString
  , wordLength :: {-# UNPACK #-} !Int
  , wordWord   :: {-# UNPACK #-} !(VU.Vector IMMC)
  }
  deriving (Show,Eq,Ord)

instance NFData Word where
  rnf !(Word {}) = ()

parseWord :: BL.ByteString -> Word
parseWord w = case ABL.eitherResult (ABL.parse go w) of
                Left  err -> error err
                Right p   -> force p
  where
    go = Word <$> AB.decimal
              <*  AB.many1 AB.space
              <*> (wW <$> wrd)
              <*> (wW <$> wrd)
              <*> AB.decimal
              <*  AB.many1 AB.space
              <*> ((VU.fromList . map wW) <$> (AB.takeWhile1 (not . AB.isHorizontalSpace) `AB.sepBy` AB.space))
    wrd  = AB.takeWhile1 (not . AB.isHorizontalSpace) <* AB.space
    wW   = immc . fromByteString

addWordDelims :: Word -> Word
addWordDelims w
  | VU.length ww >= 2 && VU.head ww == "^" && VU.last ww == "$" = w
  | otherwise = w { wordWord = "^" `VU.cons` wordWord w `VU.snoc` "$" }
  where ww = wordWord w

removeWordDelims :: Word -> Word
removeWordDelims w
  | VU.length ww >= 2 && VU.head ww == "^" && VU.last ww == "$" = w { wordWord = VU.init . VU.tail $ wordWord w }
  | otherwise = w
  where ww = wordWord w

