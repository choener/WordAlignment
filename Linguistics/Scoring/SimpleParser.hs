{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a simple scoring scheme based on pairs of unigrams.

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



-- | Score 'MultiChar's @x@ and @y@ based on the simple scoring system: (i)
-- lookup (x,y) and use the score if found; (ii) if (x,y) is not in the
-- database, then return the default matching 'defMatch' score if @x==y@,
-- otherwise return the default mismatch 'defMismatch' score.

scoreUnigram :: SimpleScoring -> InternedMultiChar -> InternedMultiChar -> Double
scoreUnigram SimpleScoring {..} x y =
  maybe (if x==y then defMatch else defMismatch)
  id
  (unsafePerformIO $ H.lookup simpleScore (x,y))
{-# INLINE scoreUnigram #-}

data SimpleScoring = SimpleScoring
  { simpleScore  :: !(H.BasicHashTable (InternedMultiChar,InternedMultiChar) Double)
  , gapScore     :: !Double
  , gapOpen      :: !Double
  , gapExtend    :: !Double
  , defMatch     :: !Double
  , defMismatch  :: !Double
  }
  deriving (Show)

data ParsedLine
  = PLset ByteString [InternedMultiChar]
  | PLeq ByteString Double
  | PLinset ByteString ByteString Double
  | PLgap Double
  | PLgapopen Double
  | PLgapextend Double
  | PLdefmatch Double
  | PLdefmismatch Double
  deriving (Show,Eq,Ord)

parseLine l = case ABL.eitherResult (ABL.parse go l) of
                Left  err -> error err
                Right p   -> p
  where go =   PLset         <$ "Set"       <*> wd <*> mc `AB.sepBy1` AB.skipSpace -- AB.skipWhile AB.isHorizontalSpace
           <|> PLeq          <$ "Eq"        <*> wd <*> nm
           <|> PLinset       <$ "InSet"     <*> wd <*> wd <*> nm
           <|> PLgap         <$ "Gap"       <*> nm
           <|> PLgapopen     <$ "GapOpen"   <*> nm
           <|> PLgapextend   <$ "GapExtend" <*> nm
           <|> PLdefmatch    <$ "Match"     <*> nm
           <|> PLdefmismatch <$ "Mismatch"  <*> nm
        wd = AB.skipSpace *> AB.takeWhile1 (not . AB.isHorizontalSpace)
        mc = fromByteString <$> wd
        nm = AB.skipSpace *> AB.double

genSimpleScoring :: BL.ByteString -> SimpleScoring
genSimpleScoring l = SimpleScoring t g dm di
  where
    t    = unsafePerformIO $ H.fromListWithSizeHint (Prelude.length ys) ys
    ls   = BL.lines l
    xs   = map parseLine ls
    ys   = concatMap genPairs $ iss ++ eqs
    sets = [s  | s@(PLset _ _)     <- xs]
    eqs  = [e  | e@(PLeq _ _)      <- xs]
    iss  = [i  | i@(PLinset _ _ _) <- xs]
    [dm] = [dm | PLdefmatch dm     <- xs]
    [di] = [di | PLdefmismatch di  <- xs]
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

