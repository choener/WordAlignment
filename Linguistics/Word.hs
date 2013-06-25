{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Linguistics.Word where

import Control.Applicative
import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AB hiding (takeWhile1)
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL hiding (readFile)
import qualified Data.Vector as V

--
--  9 Albanian_Tosk 1.214 6 \' b a lʸ t ə
--

data Word = Word
  { wordID     :: {-# UNPACK #-} !Int
  , wordLang   :: {-# UNPACK #-} !ByteString
  , wordClass  :: {-# UNPACK #-} !ByteString
  , wordLength :: {-# UNPACK #-} !Int
  , wordWord   :: {-# UNPACK #-} !(V.Vector ByteString)
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
              <*> wrd
              <*> wrd
              <*> AB.decimal
              <*  AB.many1 AB.space
              <*> (V.fromList <$> (AB.takeWhile1 (not . AB.isHorizontalSpace) `AB.sepBy` AB.space))
    wrd  = AB.takeWhile1 (not . AB.isHorizontalSpace) <* AB.space

addWordDelims :: Word -> Word
addWordDelims w = w { wordWord = V.fromList $ ["^"] ++ (V.toList $ wordWord w) ++ ["$"] }

removeWordDelims :: Word -> Word
removeWordDelims w = w { wordWord = V.init . V.tail $ wordWord w }
