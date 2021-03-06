
-- | Map between 'String's that represent characters and their 'Int'-based
-- representation.
--
-- NOTE filtering the scores list and creating a single bigram map takes about
-- 70 seconds.
--
-- NOTE A single bigram map costs around 160 MByte ram. This includes the
-- overhead for actually storing the bigrams once (creating pointers instead of
-- multiple copied 'Bigram' data structures.

module Linguistics.WordAlignment.Bigram where

import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Control.Lens
import           Data.Attoparsec.ByteString.Lazy ((<?>))
import           Data.ByteString (ByteString)
import           Data.Function
import           Data.Hashable
import           Data.Interned
import           Data.List
import           Data.Strict.Tuple
import           GHC.Generics (Generic)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AB hiding (takeWhile1)
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL hiding (readFile)
import qualified Data.ByteString.Short as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Debug.Trace
import           Text.Printf

import           NLP.Text.BTI

import           Linguistics.WordAlignment.Common



data Bigram = Bigram
  { peekChar :: {-# UNPACK #-} !BTI
  , hitChar  :: {-# UNPACK #-} !BTI
  }
  deriving (Show,Eq,Ord,Generic)

instance Hashable Bigram where
  hashWithSalt s (Bigram p h) = hashWithSalt s (p,h) -- (uninternMultiChar p, uninternMultiChar h)
  hash           (Bigram p h) = hash (hash p , hash h)
  {-# Inline hashWithSalt #-}
  {-# Inline hash         #-}

instance NFData Bigram where
  rnf !(Bigram a b) = ()

instance Hashable (Pair Int Int) where
  hashWithSalt s (a:!:b) = hashWithSalt s (a,b)
  {-# Inline hashWithSalt #-}

-- | Try to read the first line to figure out if there is a default score there

withDefault :: Double -> [BL.ByteString] -> (Double,[BL.ByteString])
withDefault d [] = (d,[])
withDefault d (x:xs)
  | [(rd,"")] <- readsPrec 0 (BL.unpack x) = (rd,xs)
  | otherwise = (d,(x:xs))

parseLine l = case ABL.eitherResult (ABL.parse go l) of
                Left  err -> error $ "Linguistics.WordAlignment.Bigram:parseLine of: " ++ show l ++ " failed with: " ++ show err
                Right p   -> force p
  where
    go     = (,,,,) <$> lang <*> lang <*> bigram <*> bigram <*> score <?> "go"
    lang   = wrd <?> "lang"
    bigram = Bigram <$> wrd <*> wrd <?> "bigram"
    score  = AB.double <?> "score"
    wrd    = btiFromCS <$> AB.takeWhile1 (not . AB.isHorizontalSpace) <* AB.skipSpace <?> "word"
{-# NoInline parseLine #-}

type Lang = BTI
type Line = (Lang, Lang, Bigram, Bigram, Double)
type Scores = HM.HashMap (Bigram:!:Bigram) Double

data Mapping = Mapping
  { bigrams :: !(M.Map Bigram Bigram)
  , lliid   :: !(M.Map (Lang:!:Lang) Scores)
  }
  deriving (Show)

instance Hashable (Pair Bigram Bigram) where
  hashWithSalt s (a:!:b) = hashWithSalt s (a,b)

-- |
--
-- This one is confusing. Given a bigram a-b :!: c-d from lang 1 to lang 2,
-- we create the bigram c-d :!: a-b from lang 2 to lang 1 as well.

lines2mapping :: [Line] -> Mapping
lines2mapping = foldl' mkMapping emptyMapping . concatMap dupGroup . groupBy ((==) `on` ((^._1) &&& (^._2))) where
  dupGroup ls@(l:_)
    | l^._1 == l^._2 = [ls]
    | otherwise      = [ls,ls']
    where ls' = map (\(l1,l2,b1,b2,d) -> (l2,l1,b2,b1,d)) . filter (\l -> l^._1 /= l^._2) $ ls

emptyMapping = let b = Bigram "" ""
               in Mapping (M.singleton b b) M.empty

-- | Barf in case incompatible duplicated bigram have been found.

mkMapping :: Mapping -> [Line] -> Mapping
mkMapping !m [] = m
mkMapping !(Mapping bs ll) xs@(x:_)
  | otherwise = Mapping bs' ll'
  where
    nom = filter (`M.notMember` bs) $ map (^._3) xs ++ map (^._4) xs
    bs' = bs `M.union` (M.fromList $ map (\a -> (a,a)) nom)
    ll' = M.insertWith HM.union (x^._1 :!: x^._2) ys ll
    ys :: Scores
    ys = HM.fromListWith insertBarf
           [ ((k1:!:k2),d)
           | y <- xs
           , let k1 = bs' M.! (y^._3)
           , let k2 = bs' M.! (y^._4)
           , let d = y ^._5
           ]
    insertBarf dup1 dup2 = error $ "the following two elements are duplicated with language pair: " ++ show (x^._1,x^._2) ++ " " ++ show dup1 ++ " " ++ show dup2

-- | Given a set of acceptable languages, a default score, and the lazy
-- bytestring of scores, create the 'Mapping' of languages and scores.

mkBigramMap :: S.Set BTI -> Double -> BL.ByteString -> Mapping
mkBigramMap langs wd b = lines2mapping xs where
  (d,ls) = withDefault wd $ BL.lines b
  xs = filter inLangSet $ map parseLine ls
  inLangSet l
    | S.null langs = True
    |  (l^._1) `S.member` langs
    && (l^._2) `S.member` langs = True
    | otherwise = False

-- | (write me)

getScores2 :: Bool -> Mapping -> Lang -> Lang -> Scores
getScores2 vrb ss a b
  | Just z <- M.lookup (a:!:b) (lliid ss) = z
  | vrb = trace (printf "Language pair %s %s not found in mapping! Returning empty hashmap\n" (toUtf8String a) (toUtf8String b))
                HM.empty
  | otherwise = HM.empty

