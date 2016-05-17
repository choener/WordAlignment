{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | A single word in a language. Uses a 'MultiChar' encoding for the actual
-- characters. MultiChar encodings need to be decoded for printing on screen.

module Linguistics.WordAlignment.Word where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Aeson
import           Data.Attoparsec.ByteString.Lazy ((<?>))
import           Data.ByteString (ByteString)
import           Data.Interned
import           Data.Interned.ByteString
import           Data.List (intersperse)
import           GHC.Generics
import           Prelude hiding (Word)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AB hiding (takeWhile1)
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL hiding (readFile)
import qualified Data.ByteString.Short as S
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           Data.Text.Encoding (encodeUtf8)

import           NLP.Text.BTI



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
  , wordClass  :: {-# UNPACK #-} !BTI -- InternedByteString
  , wordLang   :: {-# UNPACK #-} !BTI -- InternedByteString
  , wordLength :: {-# UNPACK #-} !Int
  , wordWord   :: {-# UNPACK #-} !(VU.Vector BTI)
  }
  deriving (Show,Eq,Ord,Generic)

instance ToJSON Word where
  toJSON (Word i c lang len w) = object
    [ "id"    .= i
    , "class" .= c
    , "lang"  .= lang
    , "word"  .= w
    ]

instance NFData Word where
  rnf !(Word {}) = ()

parseWord :: BL.ByteString -> Word
parseWord w = case ABL.eitherResult (ABL.parse go w) of
                Left  err -> error $ "failed to parse line: " ++ show w ++ " with error: " ++ show err
                Right p   -> force p
  where
    go = Word <$> (AB.decimal         <?> "number")
              <*  (AB.skipSpace       <?> "1+ ws")
              <*> ((wW <$> wrd)       <?> "class")
              <*> ((wW <$> wrd)       <?> "language")
              <*> (AB.decimal         <?> "length")
              <*  (AB.skipSpace       <?> "1+ ws")
              <*> ((VU.fromList . map wW) <$> (AB.takeWhile1 (not . AB.isHorizontalSpace) `AB.sepBy` AB.space) <?> "word")
    wrd  = AB.takeWhile1 (not . AB.isHorizontalSpace) <* AB.skipSpace
    wW   = btiFromCS

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

-- | Format a 'Word' as a lazy text with interspersed white space.

wordLazyTextWS :: Word -> TL.Text
wordLazyTextWS = TLB.toLazyText . wordLazyTextWSB
{-# Inline wordLazyTextWS #-}

-- | A Builder for the actual word in a 'Word'. With intersperse white
-- space.

wordLazyTextWSB :: Word -> TLB.Builder
wordLazyTextWSB = mconcat . intersperse " " . map (TLB.fromText . btiToCS) . VU.toList . wordWord
{-# Inline wordLazyTextWSB #-}

-- | A builder for an utf8 bytestring.

wordUtf8Builder :: Word -> BB.Builder
wordUtf8Builder = mconcat . intersperse " " . map (BB.byteString . btiToUtf8) . VU.toList . wordWord
{-# Inline wordUtf8Builder #-}

-- |
--
-- TODO @Builder@ or @Text@ or @Lazy.Text@ ?

data FastChars = FastChars
  { fcTable :: !(HM.HashMap BTI B.ByteString)
  , fcWidth :: !Int
  }

-- | Generate the fast lookup table

fastChars :: Int -> V.Vector Word -> FastChars
fastChars width ws = {-# SCC "fastChars" #-} deepseq ws `seq` FastChars hm width
  where hm = HM.fromList . map fmt . addDefaultChars . concatMap (VU.toList . wordWord) . V.toList $ ws
        fmt k = (k , encodeUtf8 . TL.toStrict . TLB.toLazyText . TF.left width ' ' . btiToText $ k)
        addDefaultChars xs = xs ++ map btiFromCS ["-", "$", "^" :: T.Text]
{-# NoInline fastChars #-}

fastChar :: FastChars -> BTI -> BB.Builder -- TLB.Builder
--fastChar (FastChars hm width) k = {-# SCC "fastChar" #-} maybe (BB.byteString . encodeUtf8 . TF.format "{}" . TF.left width ' ' . TF.Only $ btiToCS k) BB.byteString $ HM.lookup k hm
fastChar (FastChars hm width) k = {-# SCC "fastChar" #-} maybe encodek BB.byteString $ HM.lookup k hm
  where encodek = BB.byteString . encodeUtf8 . TL.toStrict . TF.format "{}" . TF.Only . TF.left width ' ' $ (btiToCS k :: T.Text)
{-# InlineAble fastChar #-}

-- | Word to bigram vector

wordToBigramVector :: VU.Vector BTI -> VU.Vector (BTI,BTI)
wordToBigramVector m' =
  let m = "^" `VU.cons` m' `VU.snoc` "$"
  in VU.zip m (VU.tail m)
{-# Inline wordToBigramVector #-}

