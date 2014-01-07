{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | The new simple scoring system is based on pairs of unigrams. This parser
-- reads the configuration file.
--
-- Example for such a file:
--
-- Set Vowel  a e i o u
-- Set Liquid l r
-- Eq Vowel 2
-- InSet Liquid Liquid 1
-- Gap       -3
-- GapOpen   -5
-- GapExtend -2
-- Default   -5

module Linguistics.Scoring.SimpleParser where

import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import           Data.Strict.Tuple
import           Data.Stringable
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as AB hiding (takeWhile1,skipWhile)
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.ByteString.Lazy.Char8 as BL hiding (unpack)
import qualified Data.HashTable.IO as H
import           System.IO.Unsafe

import NLP.Alphabet.MultiChar



data SimpleScoring = SimpleScoring
  { simpleScore  :: !(H.BasicHashTable (InternedMultiChar,InternedMultiChar) Double)
  , gapScore     :: !Double
  , defaultScore :: !Double
  }
  deriving (Show)

data ParsedLine
  = PLset ByteString [InternedMultiChar]
  | PLeq ByteString Double
  | PLinset ByteString ByteString Double
  | PLgap Double
  | PLgapopen Double
  | PLgapextend Double
  | PLdefault Double
  deriving (Show,Eq,Ord)

parseLine l = case ABL.eitherResult (ABL.parse go l) of
                Left  err -> error err
                Right p   -> p
  where go =   PLset       <$ "Set"       <*> wd <*> mc `AB.sepBy1` AB.skipSpace -- AB.skipWhile AB.isHorizontalSpace
           <|> PLeq        <$ "Eq"        <*> wd <*> nm
           <|> PLinset     <$ "InSet"     <*> wd <*> wd <*> nm
           <|> PLgap       <$ "Gap"       <*> nm
           <|> PLgapopen   <$ "GapOpen"   <*> nm
           <|> PLgapextend <$ "GapExtend" <*> nm
           <|> PLdefault   <$ "Default"   <*> nm
        wd = AB.skipSpace *> AB.takeWhile1 (not . AB.isHorizontalSpace)
        mc = fromByteString <$> wd
        nm = AB.skipSpace *> AB.double

genSimpleScoring :: BL.ByteString -> SimpleScoring
genSimpleScoring l = SimpleScoring t g d
  where
    t    = unsafePerformIO $ H.fromListWithSizeHint (Prelude.length ys) ys
    ls   = BL.lines l
    xs   = map parseLine ls
    ys   = concatMap genPairs $ iss ++ eqs
    sets = [s  | s@(PLset _ _)     <- xs]
    eqs  = [e  | e@(PLeq _ _)      <- xs]
    iss  = [i  | i@(PLinset _ _ _) <- xs]
    [d]  = [d  | PLdefault d       <- xs]
    [g]  = [g  | PLgap g           <- xs]
    [go] = [go | PLgapopen go      <- xs]
    [ge] = [ge | PLgapextend ge    <- xs]
    genPairs (PLeq    x   d) = let ss = lookupSet x in [ ((s,s),d) | s <- ss ]
    genPairs (PLinset x y d) = let ss = lookupSet x
                                   tt = lookupSet y in [ ((s,t),d) | s <- ss, t <- tt ]
    lookupSet k = let go [] = error $ "missing set for key: " ++ show k
                      go (PLset n xs:ss) = if k==n then xs else go ss
                  in  go sets

simpleScoreFromFile f = BL.readFile f >>= return . genSimpleScoring
