{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Arrow
import           Control.Monad (unless)
import           Control.Monad (when)
import           Control.Parallel.Strategies
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Function
import           Data.List (intersperse)
import           Data.List (tails,genericLength,genericTake,genericDrop,group)
import           Data.Ord
import           Data.Strict.Tuple
import           Data.Vector (fromList)
import           Debug.Trace (trace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashTable.IO as H
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           System.Console.CmdArgs
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Printf

import           NLP.Alphabet.MultiChar
import           NLP.Scoring.SimpleUnigram
import           NLP.Scoring.SimpleUnigram.Default
import           NLP.Scoring.SimpleUnigram.Import

import Linguistics.TwoWay
{- we test interning with TwoWay alignments only
import Linguistics.ThreeWay
import Linguistics.FourWay
-}
import Linguistics.Bigram
import Linguistics.Common
import Linguistics.Word
import Linguistics.TwoWay.AdvancedBigram

data Config
  = TwoWay
    { scoreFile :: String
    , bigramDef :: Double
    , unibiDef  :: Double
    , gapOpen :: Double
    , gapExtend :: Double
    , block :: Maybe (Integer,Integer)
    , selfAlign :: Bool
    }
  | TwoWaySimple
    { scoreFile :: String
    , block :: Maybe (Integer,Integer)
    , selfAlign :: Bool
    }
  {- NLA
  | ThreeWay
    { scoreFile :: String
    , defaultScore :: Double
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  | ThreeWaySimple
    { scoreFile :: String
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  | FourWay
    { scoreFile :: String
    , defaultScore :: Double
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  | FourWaySimple
    { scoreFile :: String
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  -}
  | Info
    {
    }
  deriving (Show,Data,Typeable)

twoway = TwoWay
  { scoreFile = "" &= help "the file to read the scores from"
  , bigramDef = (-20) &= help "score to use for unknown bigram matches"
  , unibiDef  = (-5) &= help "score to close a gap if the closing characters are unknown"
  , gapOpen = (-5) &= help "cost to open a gap"
  , gapExtend = (-1) &= help "cost to extend a gap"
  , block = Nothing &= help "when using --block N,k calculate only the k'th block (starting at 1) with length N. For parallelized computations."
  , selfAlign = False &= help "align each word with itself as well"
  } &= help "Align two words at a time for all ordered word combinations"

twowaySimple = TwoWaySimple
  { scoreFile = def &= help ""
  }

{- NLA
threeway = ThreeWay
  {
  }

threewaySimple = ThreeWaySimple
  {
  }

fourway = FourWay
  {
  }

fourwaySimple = FourWaySimple
  {
  }
-}

info = Info
  {
  }

config = [twoway,twowaySimple, {- threeway,threewaySimple,fourway,fourwaySimple, -} info]
  &= program "WordAlign"
  &= summary "WordAlign v.0.0.1"

main = do
  o <- cmdArgs $ modes config
  ws' <- BL.getContents >>= return . map parseWord . BL.lines
  case o of
    Info{} -> do
      let l :: Integer = genericLength ws'
      let c2 = l * (l-1) `div` 2 -- number of alignments
      let t2 :: Double = fromIntegral c2 / 5000 / 60 / 60 -- approximate time in hours
      let c3 = l * (l-1) * (l-2) `div` 3
      let t3 :: Double = fromIntegral c3 / 5000 / 60 / 60 -- TODO fix time constant
      let c4 = l * (l-1) * (l-2) * (l-3) `div` 4
      let t4 :: Double = fromIntegral c4 / 5000 / 60 / 60 -- TODO fix time constant
      printf "%d  %.1f    %d  %.1f    %d  %.1f\n" c2 t2 c3 t3 c4 t4
    TwoWay{..} -> do
      let ws = map addWordDelims ws'
      let bs = blockWith block $ [ (a,b) | (a:as) <- tails ws, b <- if selfAlign then (a:as) else as ]
      let chkLs = S.fromList . map wordLang $ ws
      {-
      let chkLs = if block==Nothing
                    then S.fromList . map wordLang $ ws
                    else S.fromLIst . map wordLang $ ws -- S.fromList . map head . group . map wordLang . concatMap (\(a,b) -> [a,b]) $ bs
                    -}
      ss <- BL.readFile scoreFile >>= return . generateLookups chkLs (-999999)
      let ts = map (\(a,b) -> ( [a,b], alignTwo (BigramScores {gapOpen = gapOpen, gapExtend = gapExtend, bigramDef = bigramDef, unigramDef = -999999, biUniDef = unibiDef, scores = getScores2 ss (wordLang a) (wordLang b)})
                                                (wordWord a) (wordWord b)
                              )
                    ) bs
      mapM_ (printAlignment (-2)) ts
    TwoWaySimple{..} -> do
      simpleScoring <- if null scoreFile then return $ error "scorefile missing"
                                         else simpleScoreFromFile scoreFile
      let ws = ws'
      let bs = blockWith block $ [ (a,b) | (a:as) <- tails ws, b <- if selfAlign then (a:as) else as ]
      let ts = map (\(a,b) -> ( [a,b], alignTwoSimple simpleScoring (wordWord a) (wordWord b)
                              )
                    ) bs
      mapM_ (printAlignment 0) ts
    {-
    ThreeWay{..} -> do
      let ws = map addWordDelims ws'
      let bs = blockWith block $ [ (a,b,c) | (a:as) <- tails ws, (b:bs) <- tails as, c <- bs ]
      let chkLs = S.fromList . map wordLang $ ws
      {-
      let chkLs = if block==Nothing
                    then S.fromList . map wordLang $ ws
                    --else S.fromList . map head . group . map wordLang . concatMap (\(a,b,c) -> [a,b,c]) $ bs
                    else S.fromLIst . map wordLang $ ws -- S.fromList . map head . group . map wordLang . concatMap (\(a,b) -> [a,b]) $ bs
                    -}
      ss <- BL.readFile scoreFile >>= return . generateLookups chkLs defaultScore
      let ts = ss `seq` map (\(a,b,c) -> ( [a,b,c], alignThree defaultScore gapOpen (getScores3 ss (wordLang a) (wordLang b) (wordLang c))
                                                (wordWord a) (wordWord b) (wordWord c)
                                         )
                            ) bs
      mapM_ (printAlignment (-2)) ts
    ThreeWaySimple{..} -> do
      [vwl,cns] <- readFile vowelConsonantFile >>= return . map VU.fromList . lines
      scs <- if null scoreFile then return [3,1,1,0,0,-1] else (readFile scoreFile >>= return . map read . words)
      let ws = ws'
      let bs = blockWith block $ [ (a,b,c) | (a:as) <- tails ws, (b:bs) <- tails as, c <- bs ]
      let ts = map (\(a,b,c) -> ( [a,b,c], alignThreeSimple vwl cns scs gapOpen (wordWord a) (wordWord b) (wordWord c)
                                )
                    ) bs
      mapM_ (printAlignment 0) ts
    FourWay{..} -> do
      let ws = map addWordDelims ws'
      let bs = blockWith block $ [ (a,b,c,d) | (a:as) <- tails ws, (b:bs) <- tails as, (c:cs) <- tails bs, d <- cs ]
      let chkLs = S.fromList . map wordLang $ ws
      {-
      let chkLs = if block==Nothing
                    then S.fromList . map wordLang $ ws
                    --else S.fromList . map head . group . map wordLang . concatMap (\(a,b,c,d) -> [a,b,c,d]) $ bs
                    else S.fromLIst . map wordLang $ ws -- S.fromList . map head . group . map wordLang . concatMap (\(a,b) -> [a,b]) $ bs
                    -}
      ss <- BL.readFile scoreFile >>= return . generateLookups chkLs defaultScore
      let ts = map (\(a,b,c,d) -> ( [a,b,c,d], alignFour defaultScore gapOpen (getScores4 ss (wordLang a) (wordLang b) (wordLang c) (wordLang d))
                                                (wordWord a) (wordWord b) (wordWord c) (wordWord d)
                                  )
                    ) bs
      mapM_ (printAlignment (-2)) ts
    FourWaySimple{..} -> do
      [vwl,cns] <- readFile vowelConsonantFile >>= return . map VU.fromList . lines
      scs <- if null scoreFile then return [3,1,1,0,0,-1] else (readFile scoreFile >>= return . map read . words)
      let ws = ws'
      let bs = blockWith block $ [ (a,b,c,d) | (a:as) <- tails ws, (b:bs) <- tails as, (c:cs) <- tails bs, d <- cs ]
      let ts = map (\(a,b,c,d) -> ( [a,b,c,d], alignFourSimple vwl cns scs gapOpen (wordWord a) (wordWord b) (wordWord c) (wordWord d)
                                  )
                    ) bs
      mapM_ (printAlignment 0) ts
      -}

alignTwo :: BigramScores -> V.Vector InternedMultiChar -> V.Vector InternedMultiChar -> (Double, [[String]])
alignTwo scores x y
  = second (map alignPretty) -- . filter (any (\c -> c/= "$" && c/= "^")))
  $ runBigram scores 1 x y

alignTwoSimple
  :: SimpleScoring
  -> V.Vector InternedMultiChar
  -> V.Vector InternedMultiChar
  -> (Double, [[String]])
alignTwoSimple simpleScoring x y = second (map alignPretty) $ twoWaySimple simpleScoring x y

{-
alignThree :: Double -> Double -> (Scores,Scores,Scores) -> V.Vector ByteString -> V.Vector ByteString -> V.Vector ByteString -> (Double, [[String]])
alignThree sDef sGapOpen scores x y z = second (map (alignPretty . map (filter (\c -> c/= "$" && c/="^")) . tup3List)) $ threeWayBigram sDef sGapOpen scores x y z

alignThreeSimple
  :: VU.Vector Char
  -> VU.Vector Char
  -> [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> (Double, [[String]])
alignThreeSimple v c scores sGapOpen x y z = second (map (alignPretty . tup3List)) $ threeWaySimple v c scores sGapOpen x y z

alignFour :: Double -> Double -> (Scores,Scores,Scores,Scores,Scores,Scores) -> V.Vector ByteString -> V.Vector ByteString -> V.Vector ByteString -> V.Vector ByteString -> (Double, [[String]])
alignFour sDef sGapOpen scores w x y z = second (map (alignPretty . map (filter (\c -> c/= "$" && c/="^")) . tup4List)) $ fourWayBigram sDef sGapOpen scores w x y z

alignFourSimple
  :: VU.Vector Char
  -> VU.Vector Char
  -> [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> (Double, [[String]])
alignFourSimple v c scores sGapOpen w x y z = second (map (alignPretty . tup4List)) $ fourWaySimple v c scores sGapOpen w x y z
-}

blockWith Nothing      xs = xs
blockWith (Just (l,k)) xs = genericTake l . genericDrop (l * (k-1)) $ xs

getScores2 :: Mapping -> Lang -> Lang -> Scores
getScores2 ss a b
  | Just z <- M.lookup (a:!:b) (lliid ss) = z
  | otherwise = trace (printf "Language pair %s %s not found in mapping! Returning empty hashmap\n" (toUtf8String a) (toUtf8String b))
                (unsafePerformIO H.new)

{-
getScores3 :: Mapping -> Lang -> Lang -> Lang -> (Scores,Scores,Scores)
getScores3 ss a b c = (getScores2 ss a b, getScores2 ss a c, getScores2 ss b c)  -- (lliid ss M.! (a:!:b), lliid ss M.! (a:!:c), lliid ss M.! (b:!:c))

getScores4 :: Mapping -> Lang -> Lang -> Lang -> Lang -> (Scores,Scores,Scores,Scores,Scores,Scores)
getScores4 ss a b c d = (getScores2 ss a b, getScores2 ss a c, getScores2 ss a d, getScores2 ss b c, getScores2 ss b d, getScores2 ss c d)
-- (lliid ss M.! (a:!:b), lliid ss M.! (a:!:c), lliid ss M.! (a:!:d), lliid ss M.! (b:!:c), lliid ss M.! (b:!:d), lliid ss M.! (c:!:d))
-}

printAlignment :: Double -> ([Word], (Double, [[String]])) -> IO ()
printAlignment k (ws,(s,[])) = do
  printf "DEBUG!\nScore: %f\nDEBUG!\n\n" s

printAlignment k (ws,(s,(x:xs))) = do
  let ids = concat . intersperse " " . map (show . wordID)   $ ws
  let wds = concat . intersperse "   WORD   " . map (concat . intersperse " " . map toUtf8String . V.toList . wordWord) $ ws
  let ns = s / (maximum $ 1 : map ((+k) . fromIntegral . V.length . wordWord) ws)
  printf "IDS: %s SCORE: %.2f NSCORE: %.2f    WORDS: %s\n" ids s ns wds
  mapM_ putStrLn x
  putStrLn ""


tup2List (a,b) = [a,b]
tup3List (a,b,c) = [a,b,c]
tup4List (a,b,c,d) = [a,b,c,d]

