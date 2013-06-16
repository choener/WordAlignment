{-# LANGUAGE DeriveGeneric #-}
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
import Data.List
import Data.Function
import qualified Data.Vector as V
import Data.Either
import qualified Data.HashMap.Strict as H
import GHC.Generics (Generic)
import Data.Hashable
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.Attoparsec.ByteString.Char8 as AB hiding (takeWhile1)
import Control.DeepSeq
import System.Mem
import System.IO.Unsafe



type CharIntMap = M.Map String Int
type IntCharMap = M.Map Int String

createMaps :: [String] -> (CharIntMap,IntCharMap)
createMaps xs = let m = M.fromList $ zip xs [0..] in (m, M.fromList . map swap $ M.assocs m)



-- | Read the scores into an associated map

data Scores k = Scores
  { scores :: !(H.HashMap (Bigram , Bigram) Double)
  , def    :: !(Double)
  } deriving (Show)

-- | This parser efficiently parses a score file (theses things are big!)

parseScoreFile :: Double -> T.Text -> M.Map (Text,Text) (Scores Text)
parseScoreFile def t = case P.parseOnly go t of -- P.eitherResult (P.parse go t) of
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
      mk !d !m !x = M.alter altering (cla, clb) m
        where (((la,a):!:(lb,b)),c) = x
              a' = a -- Bigram { peekChar = T.copy $ peekChar a, hitChar = T.copy $ hitChar a }
              b' = b -- Bigram { peekChar = T.copy $ peekChar b, hitChar = T.copy $ hitChar b }
              cla = la -- T.copy la
              clb = lb -- T.copy lb
              altering Nothing
                = Just $ Scores (H.singleton (a',b') c) d
              altering (Just (Scores im d))
                = Just $ Scores (H.insert (a',b') c im) d

data Bigram = Bigram
  { peekChar :: {-# UNPACK #-} !Text
  , hitChar  :: {-# UNPACK #-} !Text
  }
  deriving (Show,Eq,Ord,Generic)

instance Hashable Bigram


--
--  9 Albanian_Tosk 1.214 6 \' b a lʸ t ə
--

data Word = Word
  { wordID     :: {-# UNPACK #-} !Int
  , wordLang   :: {-# UNPACK #-} !Text
  , wordClass  :: {-# UNPACK #-} !Text
  , wordLength :: {-# UNPACK #-} !Int
  , wordWord   :: {-# UNPACK #-} !(V.Vector Text)
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


test :: M.Map Int Int
test = M.fromList [ (i,i) | i <- [ 1 .. 4000000 ] ]


withDefault :: Double -> [BL.ByteString] -> (Double,[BL.ByteString])
withDefault d xs = (d,xs)

--parseLine :: BL.ByteString -> (
parseLine l = case ABL.eitherResult (ABL.parse go l) of
                Left  err -> error err
                Right p   -> p
  where
    go  = (\(!a) (!b) (!c) -> ((a,b),c)) <$> big <*> big <*> AB.double -- <?> "one bigram score line"
    big = (\(!l) (!p) (!h) -> (l,BiG p h)) <$> wrd <*> wrd <*> wrd -- <?> "on bigram"
    wrd = B.copy <$> AB.takeWhile1 (not . AB.isHorizontalSpace) <* AB.space

type Lang = B.ByteString
type LangBiG = (B.ByteString,BiG)
type Line = ((LangBiG, LangBiG), Double)
type LLBB = M.Map (Lang:!:Lang) (V.Vector BiGBiGD)

data BiGBiGD = BBD !BiG !BiG !Double
  deriving Show

data VorS a
  = V !(V.Vector a) ![a] !Int
  | S !a

getVector :: VorS a -> V.Vector a
getVector !(V v l c) | V.null v  = V.fromListN c l
                     | otherwise = v V.++ (V.fromListN c l)
getVector !(S s) = V.singleton s

vors :: VorS a -> VorS a -> VorS a
vors !(V v1 l1 c1) !(V v2 l2 c2) = V (V.concat [v1, v2, V.fromListN c1 $ reverse l1, V.fromListN c2 $ reverse l2]) [] 0
vors !(V v l c   ) !(S s       ) | c<cMax    = V v (s:l) (c+1)
                                 | otherwise = V (v V.++ (V.fromListN (c+1) $ reverse (s:l))) [] 0
vors !(S s       ) !(V v l c   ) | c<cMax    = V v (s:l) (c+1)
                                 | otherwise = V (v V.++ (V.fromListN (c+1) $ reverse (s:l))) [] 0
vors !(S s1) !(S s2) = V V.empty [s2,s1] 2

cMax = 1000

data Mapping = Mapping
  { big2int :: !(M.Map BiG Int)
  , int2big :: !(M.Map Int BiG)
  , lliid   :: !(M.Map (Lang:!:Lang) (M.Map (Int:!:Int) Double))
  }

lines2mapping :: [Line] -> Mapping
lines2mapping = addGrp mdef . groupBy ((==) `on` langPair) where
  mdef = Mapping (M.singleton (BiG "" "") 0) (M.singleton 0 (BiG "" "")) M.empty
  langPair (((l1,_),(l2,_)),_) = (l1,l2)
  addGrp m [] = m
  addGrp m ([]:xs) = addGrp m xs
  addGrp m (x:xs) = addGrp (go m x) xs
  go m [] = m
  go m xs@(x:_) = goLang m (langPair x) $ map bbd xs where
  bbd (((_,b1),(_,b2)),d) = (b1,b2,d)
  goLang m (l1,l2) [] = m

lines2languages :: [Line] -> LLBB -- M.Map (Lang,Lang) [BiGBiGD]
lines2languages = M.map getVector . M.fromListWith vors . map l2l where
  l2l :: Line -> ( (Lang:!:Lang), VorS BiGBiGD )
  l2l (((l1,b1),(l2,b2)),d) = ( (l1:!:l2), S $ BBD b1 b2 d )

generateLookups :: BL.ByteString -> LLBB -- V.Vector (((B.ByteString,BiG),(B.ByteString,BiG)),Double)
generateLookups t = lines2languages xs where
  (d,ls) = withDefault (-42) $ BL.lines t
  xs = map parseLine ls


data BiG = BiG
  { peekChr :: {-# UNPACK #-} !B.ByteString
  , hitChr  :: {-# UNPACK #-} !B.ByteString
  }
  deriving (Show,Eq,Ord)
