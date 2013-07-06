{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow
import Control.Monad (when)
import Control.Parallel.Strategies
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Function
import Data.List (intersperse)
import Data.List (tails,genericLength,genericTake,genericDrop,group)
import Data.Ord
import Data.Strict.Tuple
import Data.Vector (fromList)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashTable.IO as H
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Console.CmdArgs
import Text.Printf

import Linguistics.TwoWay
import Linguistics.ThreeWay
import Linguistics.FourWay
import Linguistics.Bigram
import Linguistics.Word
import Linguistics.Common

data Config
  = TwoWay
    { scoreFile :: String
    , defaultScore :: Double
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  | TwoWaySimple
    { scores :: [Double]
    , gapOpen :: Double
    , vowelConsonantFile :: String
    , block :: Maybe (Integer,Integer)
    }
  | ThreeWay
    { scoreFile :: String
    , defaultScore :: Double
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  | ThreeWaySimple
    { scores :: [Double]
    , gapOpen :: Double
    , vowelConsonantFile :: String
    , block :: Maybe (Integer,Integer)
    }
  | FourWay
    { scoreFile :: String
    , defaultScore :: Double
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  | FourWaySimple
    { scores :: [Double]
    , gapOpen :: Double
    , vowelConsonantFile :: String
    , block :: Maybe (Integer,Integer)
    }
  | Info
    {
    }
  deriving (Show,Data,Typeable)

twoway = TwoWay
  { scoreFile = "" &= help "the file to read the scores from"
  , defaultScore = (-42) &= help "score to use for unknown bigram matches"
  , gapOpen = (-1) &= help "cost to open a gap"
  , block = Nothing &= help "when using --block N,k calculate only the k'th block (starting at 1) with length N. For parallelized computations."
  } &= help "Align two words at a time for all ordered word combinations"

twowaySimple = TwoWaySimple
  { scores = [3,1,1,0,0,-1] &= help ""
  , vowelConsonantFile = "vowel-consonant.txt" &= help "a file defining what is a vowel and what is a consonant"
  , gapOpen = -1
  }

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

info = Info
  {
  }

config = [twoway,twowaySimple,threeway,threewaySimple,fourwaySimple,info]
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
      let bs = blockWith block $ [ (a,b) | (a:as) <- tails ws, b <- as ]
      let chkLs = if block==Nothing
                    then S.fromList . map wordLang $ ws
                    else S.fromList . map head . group . map wordLang . concatMap (\(a,b) -> [a,b]) $ bs
      ss <- BL.readFile scoreFile >>= return . generateLookups chkLs defaultScore
      let ts = map (\(a,b) -> ( [a,b], alignTwo defaultScore gapOpen (getScores2 ss (wordLang a) (wordLang b))
                                                (wordWord a) (wordWord b)
                              )
                    ) bs
      mapM_ (printAlignment (-2)) ts
    TwoWaySimple{..} -> do
      [v,c] <- readFile vowelConsonantFile >>= return . map VU.fromList . lines
      let ws = ws'
      let bs = blockWith block $ [ (a,b) | (a:as) <- tails ws, b <- as ]
      let ts = map (\(a,b) -> ( [a,b], alignTwoSimple v c scores gapOpen (wordWord a) (wordWord b)
                              )
                    ) bs
      mapM_ (printAlignment 0) ts
    ThreeWay{..} -> do
      let ws = map addWordDelims ws'
      let bs = blockWith block $ [ (a,b,c) | (a:as) <- tails ws, (b:bs) <- tails as, c <- bs ]
      let chkLs = if block==Nothing
                    then S.fromList . map wordLang $ ws
                    else S.fromList . map head . group . map wordLang . concatMap (\(a,b,c) -> [a,b,c]) $ bs
      ss <- BL.readFile scoreFile >>= return . generateLookups chkLs defaultScore
      let ts = map (\(a,b,c) -> ( [a,b,c], alignThree defaultScore gapOpen (getScores3 ss (wordLang a) (wordLang b) (wordLang c))
                                                (wordWord a) (wordWord b) (wordWord c)
                                )
                    ) bs
      mapM_ (printAlignment (-2)) ts
    ThreeWaySimple{..} -> do
      [vwl,cns] <- readFile vowelConsonantFile >>= return . map VU.fromList . lines
      let ws = ws'
      let bs = blockWith block $ [ (a,b,c) | (a:as) <- tails ws, (b:bs) <- tails as, c <- bs ]
      let ts = map (\(a,b,c) -> ( [a,b,c], alignThreeSimple vwl cns scores gapOpen (wordWord a) (wordWord b) (wordWord c)
                                )
                    ) bs
      mapM_ (printAlignment 0) ts
    FourWay{..} -> do
      let ws = map addWordDelims ws'
      let bs = blockWith block $ [ (a,b,c,d) | (a:as) <- tails ws, (b:bs) <- tails as, (c:cs) <- tails bs, d <- cs ]
      let chkLs = if block==Nothing
                    then S.fromList . map wordLang $ ws
                    else S.fromList . map head . group . map wordLang . concatMap (\(a,b,c,d) -> [a,b,c,d]) $ bs
      ss <- BL.readFile scoreFile >>= return . generateLookups chkLs defaultScore
      let ts = map (\(a,b,c,d) -> ( [a,b,c,d], alignFour defaultScore gapOpen (getScores4 ss (wordLang a) (wordLang b) (wordLang c) (wordLang d))
                                                (wordWord a) (wordWord b) (wordWord c) (wordWord d)
                                  )
                    ) bs
      mapM_ (printAlignment (-2)) ts
    FourWaySimple{..} -> do
      [vwl,cns] <- readFile vowelConsonantFile >>= return . map VU.fromList . lines
      let ws = ws'
      let bs = blockWith block $ [ (a,b,c,d) | (a:as) <- tails ws, (b:bs) <- tails as, (c:cs) <- tails bs, d <- cs ]
      let ts = map (\(a,b,c,d) -> ( [a,b,c,d], alignFourSimple vwl cns scores gapOpen (wordWord a) (wordWord b) (wordWord c) (wordWord d)
                                  )
                    ) bs
      mapM_ (printAlignment 0) ts


alignTwo :: Double -> Double -> Scores -> V.Vector ByteString -> V.Vector ByteString -> (Double, [[String]])
alignTwo sDef sGapOpen scores x y = second (map (alignPretty . map (filter (\c -> c/= "$" && c/="^")) . tup2List)) $ twoWayBigram sDef sGapOpen scores x y

alignTwoSimple
  :: VU.Vector Char
  -> VU.Vector Char
  -> [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> (Double, [[String]])
alignTwoSimple v c scores sGapOpen x y = second (map (alignPretty . tup2List)) $ twoWaySimple v c scores sGapOpen x y

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

blockWith Nothing      xs = xs
blockWith (Just (l,k)) xs = genericTake l . genericDrop (l * (k-1)) $ xs

getScores2 :: Mapping -> Lang -> Lang -> Scores
getScores2 ss a b = lliid ss M.! (a:!:b)

getScores3 :: Mapping -> Lang -> Lang -> Lang -> (Scores,Scores,Scores)
getScores3 ss a b c = (lliid ss M.! (a:!:b), lliid ss M.! (a:!:c), lliid ss M.! (b:!:c))

getScores4 :: Mapping -> Lang -> Lang -> Lang -> Lang -> (Scores,Scores,Scores,Scores,Scores,Scores)
getScores4 ss a b c d = (lliid ss M.! (a:!:b), lliid ss M.! (a:!:c), lliid ss M.! (a:!:d), lliid ss M.! (b:!:c), lliid ss M.! (b:!:d), lliid ss M.! (c:!:d))

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
