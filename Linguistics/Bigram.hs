{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

-- | Map between 'String's that represent characters and their 'Int'-based
-- representation.
--
-- NOTE filtering the scores list and creating a single bigram map takes about
-- 70 seconds.
--
-- NOTE A single bigram map costs around 160 MByte ram. This includes the
-- overhead for actually storing the bigrams once (creating pointers instead of
-- multiple copied 'Bigram' data structures.

module Linguistics.Bigram where

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
import qualified Data.ByteString.Lazy as BL hiding (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL hiding (readFile)
--import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import Control.Lens
import Control.Arrow
import qualified Data.HashTable.IO as H
import System.IO.Unsafe
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S



data Bigram = Bigram
  { peekChar :: {-# UNPACK #-} !ByteString
  , hitChar  :: {-# UNPACK #-} !ByteString
  }
  deriving (Show,Eq,Ord,Generic)

instance Hashable Bigram where
  hashWithSalt s (Bigram p h) = hashWithSalt s (p,h)

instance NFData Bigram where
  rnf (Bigram !a !b) = ()

instance Hashable (Pair Int Int) where
  hashWithSalt s (a:!:b) = hashWithSalt s (a,b)


withDefault :: Double -> [BL.ByteString] -> (Double,[BL.ByteString])
withDefault d [] = (d,[])
withDefault d (x:xs)
  | [(rd,"")] <- readsPrec 0 (BL.unpack x) = (rd,xs)
  | otherwise = (d,(x:xs))

parseLine l = case ABL.eitherResult (ABL.parse go l) of
                Left  err -> error err
                Right p   -> force p
  where
    go  = (\(l1,b1) (l2,b2) d -> (l1,l2,b1,b2,d)) <$> big <*> big <*> AB.double -- <?> "one bigram score line"
    big = (\l p h -> (l,Bigram p h)) <$> wrd <*> wrd <*> wrd -- <?> "on bigram"
    wrd = B.copy <$> AB.takeWhile1 (not . AB.isHorizontalSpace) <* AB.space


type Lang = B.ByteString
type Line = (Lang, Lang, Bigram, Bigram, Double)
type Scores = H.BasicHashTable {- M.Map -} (Bigram:!:Bigram) Double

data Mapping = Mapping
  { bigrams :: !(M.Map Bigram Bigram)
  , lliid   :: !(M.Map (Lang:!:Lang) Scores)
  }
  deriving (Show)

instance Hashable (Pair Bigram Bigram) where
  hashWithSalt s (a:!:b) = hashWithSalt s (a,b)

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
    ll' = M.insertWith theUnion (x^._1 :!: x^._2) ys ll
    theUnion :: Scores -> Scores -> Scores
    theUnion a b = unsafePerformIO $ do
      a' <- H.toList a
      b' <- H.toList b
      H.fromList $ a' ++ b'
    ys :: Scores
    ys = unsafePerformIO $ do
          H.fromListWithSizeHint (length xs)
           [ ((k1:!:k2),d)
           | y <- xs
           , let k1 = bs' M.! (y^._3)
           , let k2 = bs' M.! (y^._4)
           , let d = y ^._5
           ]
    {-
    ys = M.fromList $ [ ((k1:!:k2),d)
           | y <- xs
           , let k1 = bs' M.! (y^._3)
           , let k2 = bs' M.! (y^._4)
           , let d = y ^._5
           ] -}

generateLookups :: S.Set B.ByteString -> Double -> BL.ByteString -> Mapping
generateLookups langs wd b = lines2mapping xs where
  (d,ls) = withDefault wd $ BL.lines b
  xs = filter inLangSet $ map parseLine ls
  inLangSet l
    | S.null langs = True
    | (l^._1) `S.member` langs && (l^._2) `S.member` langs = True
    | otherwise = False

test = do
  xs <- BL.readFile "sc01M" >>= return . generateLookups S.empty (-42)
  print xs

