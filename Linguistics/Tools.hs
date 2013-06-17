{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

-- | Map between 'String's that represent characters and their 'Int'-based
-- representation.

module Linguistics.Tools where

import Control.Applicative
import Control.DeepSeq
import Data.Char (isSpace)
import Data.Either
import Data.Function
import Data.Hashable
import Data.List
import Data.Strict.Tuple
import Data.Text (Text(..))
import Data.Tuple (swap)
import GHC.Generics (Generic)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AB hiding (takeWhile1)
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL hiding (readFile)
--import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import Control.Lens
import Control.Arrow
import qualified Data.HashTable.IO as H
import System.IO.Unsafe
import Data.Word
import qualified Data.IntMap.Strict as IM
import Data.Bits
import Foreign.Storable



data Bigram = Bigram
  { peekChar :: {-# UNPACK #-} !ByteString
  , hitChar  :: {-# UNPACK #-} !ByteString
  }
  deriving (Show,Eq,Ord,Generic)

instance NFData Bigram where
  rnf (Bigram !a !b) = ()

instance Hashable (Pair Int Int) where
  hashWithSalt s (a:!:b) = hashWithSalt s (a,b)


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

{-
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
-}

withDefault :: Double -> [BL.ByteString] -> (Double,[BL.ByteString])
withDefault d xs = (d,xs)

parseLine l = case ABL.eitherResult (ABL.parse go l) of
                Left  err -> error err
                Right p   -> force p
  where
    go  = (\(l1,b1) (l2,b2) d -> (l1,l2,b1,b2,d)) <$> big <*> big <*> AB.double -- <?> "one bigram score line"
    big = (\l p h -> (l,Bigram p h)) <$> wrd <*> wrd <*> wrd -- <?> "on bigram"
    wrd = B.copy <$> AB.takeWhile1 (not . AB.isHorizontalSpace) <* AB.space


type Lang = B.ByteString
type Line = (Lang, Lang, Bigram, Bigram, Double)

data Mapping = Mapping
  { bigrams :: !(M.Map Bigram Bigram)
  , lliid   :: !(M.Map (Lang:!:Lang) (M.Map (Bigram:!:Bigram) Double))
  }
  deriving (Show)


lines2mapping :: [Line] -> Mapping
lines2mapping = foldl' mkMapping emptyMapping . groupBy ((==) `on` ((^._1) &&& (^._2)))

emptyMapping = let b = Bigram "" ""
               in Mapping (M.singleton b b) M.empty

mkMapping :: Mapping -> [Line] -> Mapping
mkMapping !m [] = m
mkMapping !(Mapping bs ll) xs@(x:_)
  | otherwise = Mapping bs' ll'
  where
    nom = filter (`M.notMember` bs) $ map (^._3) xs ++ map (^._4) xs
    bs' = bs `M.union` (M.fromList $ map (\a -> (a,a)) nom)
    ll' = M.insertWith M.union (x^._1 :!: x^._2) ys ll
    ys = M.fromList $ [ ((k1:!:k2),d)
           | y <- xs
           , let k1 = bs' M.! (y^._3)
           , let k2 = bs' M.! (y^._4)
           , let d = y ^._5
           ]
{-
    (maxk,_) = M.findMax i2b
    b2i' = b2i `M.union` (M.fromList nom)
    i2b' = M.fromList $ map swap $ M.toList b2i'
    ll' = M.insertWith IM.union (x^._1 :!: x^._2) ys ll
    ys = IM.fromList $ [ ((shiftL i1 btsz .|. i2),d)
                       | y <- xs
                       , let i1 = b2i' M.! (y^._3)
                       , let i2 = b2i' M.! (y^._4)
                       , let d  = y^._5
                       ]
    btsz = bitSize (undefined :: Int) `div` 2 -1
-}


generateLookups t = lines2mapping xs where
  (d,ls) = withDefault (-42) $ BL.lines t
  xs = map parseLine ls

test = do
  xs <- BL.readFile "sc01M" >>= return . generateLookups
  print xs



{-
data Mapping = Mapping
  { big2int :: !(M.Map Bigram Int)
  , int2big :: !(M.Map Int Bigram)
  , lliid   :: !(M.Map (Lang:!:Lang) (IM.IntMap Double)) -- (M.Map (Word32:!:Word32) Double))
  }
  deriving (Show)

lines2mapping :: [Line] -> Mapping
lines2mapping = foldl' mkMapping emptyMapping . groupBy ((==) `on` ((^._1) &&& (^._2)))

emptyMapping = Mapping (M.singleton (Bigram "" "") 0) (M.singleton 0 (Bigram "" "")) M.empty

mkMapping :: Mapping -> [Line] -> Mapping
mkMapping !m [] = m
mkMapping !(Mapping b2i i2b ll) xs@(x:_)
  | M.size b2i' > 2^btsz = error "to many bigrams in mkMapping!"
  | otherwise = Mapping b2i' i2b' ll'
  where
    nom = zip (filter (`M.notMember` b2i) $ map (^._3) xs ++ map (^._4) xs) [maxk+1 ..]
    (maxk,_) = M.findMax i2b
    b2i' = b2i `M.union` (M.fromList nom)
    i2b' = M.fromList $ map swap $ M.toList b2i'
    ll' = M.insertWith IM.union (x^._1 :!: x^._2) ys ll
    ys = IM.fromList $ [ ((shiftL i1 btsz .|. i2),d)
                       | y <- xs
                       , let i1 = b2i' M.! (y^._3)
                       , let i2 = b2i' M.! (y^._4)
                       , let d  = y^._5
                       ]
    btsz = bitSize (undefined :: Int) `div` 2 -1
-}







{-
type LangBiG = (B.ByteString,BiG)
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
-}
