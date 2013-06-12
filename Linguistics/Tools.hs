{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

-- | Map between 'String's that represent characters and their 'Int'-based
-- representation.

module Linguistics.Tools where

import qualified Data.Map.Strict as M
import Data.Tuple (swap)
import Control.Applicative
import qualified Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Text.Lazy as PL
import Data.Attoparsec.Text ((<?>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.IO as T
import Data.Text (Text(..))
import Data.Char (isSpace)
import Data.Strict.Tuple
import Data.List (foldl')
import qualified Data.Vector as V
import Data.Either



type CharIntMap = M.Map String Int
type IntCharMap = M.Map Int String

createMaps :: [String] -> (CharIntMap,IntCharMap)
createMaps xs = let m = M.fromList $ zip xs [0..] in (m, M.fromList . map swap $ M.assocs m)



-- | Read the scores into an associated map
--
-- TODO strictify

data Scores k = Scores
  { scores :: !(M.Map (Bigram :!: Bigram) Double)
  , def    :: !(Double)
  } deriving (Show)

{-
readScoreFile :: Double -> FilePath -> IO Scores
readScoreFile md fp = do
  ss <- lines <$> getContents
  let (d,ts) = case (map words ss,md) of
                 ([dflt]:xs,_)  -> (read dflt, map wordsToEntry xs)
                 (xs,dflt)      -> (dflt     , map wordsToEntry xs)
  let s = Scores (M.fromList ts) d
  return s
-}

{-
wordsToEntry :: [String] -> ( ((Int,Int),(Int,Int)) , Double )
wordsToEntry [a,b,c,d,s] = ( ((a',b'),(c',d')) , s' ) where
  a' = read a
  b' = read b
  c' = read c
  d' = read d
  s' = read s
-}

-- | This parser efficiently parses a score file (theses things are big!)

parseScoreFile :: Double -> TL.Text -> M.Map (Text,Text) (Scores Text)
parseScoreFile def t = case PL.eitherResult (PL.parse go t) of
                         Left  err -> error err
                         Right p   -> p
  where
    go = f <$> (P.double <* P.endOfLine <|> pure def)
           <*> aline `P.sepBy'` P.endOfLine
           <*  (P.endOfLine <|> pure ())
           <*  P.endOfInput where
      aline  = (\a b c -> ((a:!:b),c)) <$> bigram <*> bigram <*> P.double <?> "one bigram-bigram score line"
      bigram = (\l p h -> (l,Bigram p h)) <$> wrd <*> wrd <*> wrd <?> "one bigram"
      wrd    = P.takeWhile1 (not . P.isHorizontalSpace) <* P.space
      f d xs = foldl' (mk d) (M.empty :: M.Map (Text,Text) (Scores Text)) xs -- Scores (M.fromList xs) d
      mk d m x = M.alter altering (la, lb) m
        where (((la,a):!:(lb,b)),c) = x
              a' = Bigram { peekChar = T.copy $ peekChar a, hitChar = T.copy $ hitChar a }
              b' = Bigram { peekChar = T.copy $ peekChar b, hitChar = T.copy $ hitChar b }
              altering Nothing
                = Just $ Scores (M.singleton (a':!:b') c) d
              altering (Just (Scores im d))
                = Just $ Scores (M.insert (a':!:b') c im) d

data Bigram = Bigram
  { peekChar :: !Text
  , hitChar  :: !Text
  }
  deriving (Show,Eq,Ord)


--
--  9 Albanian_Tosk 1.214 6 \' b a lʸ t ə
--

data Word = Word
  { wordID     :: !Int
  , wordLang   :: !Text
  , wordClass  :: !Text
  , wordLength :: !Int
  , wordWord   :: !(V.Vector Text)
  }
  deriving (Show,Eq,Ord)

wordParser :: [TL.Text] -> [Word]
wordParser ts = let (ls,rs) = partitionEithers . map (PL.eitherResult . PL.parse go) $ ts
                in if null ls then rs
                              else error $ show ls
  where
    go = word
    word = Word <$> P.decimal
                <*  P.many1 P.space
                <*> wrd
                <*> wrd
                <*> P.decimal
                <*  P.many1 P.space
                <*> (V.fromList <$> (P.takeWhile1 (not . isSpace) `P.sepBy` P.space))
    wrd  = P.takeWhile1 (not . isSpace) <* P.space

