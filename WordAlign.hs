{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Console.CmdArgs
import Data.List (tails,genericLength,genericTake,genericDrop,group)
import Text.Printf
import Control.Arrow
import Control.Parallel.Strategies
import Data.Vector (fromList)
import qualified Data.Map.Strict as M
import Data.List (intersperse)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Strict.Tuple
import qualified Data.Set as S
import Control.Monad (when)
import qualified Data.HashTable.IO as H
import qualified Data.List as L
import Data.Ord
import Data.Function
import Data.ByteString (ByteString)

import Linguistics.TwoWay
--import Linguistics.FourWay
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
  | FourWay
    { scoreFile :: String
    , defaultScore :: Double
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  | Info
    {
    }
  deriving (Show,Data,Typeable)

twoway = TwoWay
  { scoreFile = "" &= help "the file to read the scores from"
  , defaultScore = (-42) &= help "score to use for unknown bigram matches"
  , gapOpen = 0 &= help "cost to open a gap"
  , block = Nothing
  }

fourway = FourWay
  {
  }

info = Info
  {
  }

main = do
  o <- cmdArgs $ modes [twoway,fourway,info]
  ws <- BL.getContents >>= return . map parseWord . BL.lines
  case o of
    Info{} -> do
      let l :: Integer = genericLength ws
      let c2 = l * (l-1) `div` 2 -- number of alignments
      let t2 :: Double = fromIntegral c2 / 5000 / 60 / 60 -- approximate time in hours
      let c3 = l * (l-1) * (l-2) `div` 3
      let t3 :: Double = fromIntegral c3 / 5000 / 60 / 60 -- TODO fix time constant
      let c4 = l * (l-1) * (l-2) * (l-3) `div` 4
      let t4 :: Double = fromIntegral c4 / 5000 / 60 / 60 -- TODO fix time constant
      printf "%d  %.1f    %d  %.1f    %d  %.1f\n" c2 t2 c3 t3 c4 t4
    TwoWay{..} -> do
      let bs = blockWith block $ [ (a,b) | (a:as) <- tails ws, b <- as ]
      let chkLs = if block==Nothing
                    then S.fromList . map wordLang $ ws
                    else S.fromList . map head . group . map wordLang . concatMap (\(a,b) -> [a,b]) $ bs
      ss <- BL.readFile scoreFile >>= return . generateLookups chkLs defaultScore
      let ts = map (\(a,b) -> ( [a,b], alignTwo defaultScore gapOpen (getScores2 ss (wordLang a) (wordLang b))
                                                (wordWord a) (wordWord b)
                              )
                    ) bs
      mapM_ printAlignment ts

alignTwo :: Double -> Double -> Scores -> V.Vector ByteString -> V.Vector ByteString -> (Double, [[String]])
alignTwo sDef sGapOpen scores x y = second (map tup2List) $ nWay2 sDef sGapOpen scores x y

{-
    FourWay{..} -> do
      let ts = [ second (map tup4List) $ nWay4 a b c d | (a:as) <- tails ls
                                                       , (b:bs) <- tails as
                                                       , (c:cs) <- tails bs
                                                       , d      <- cs
                                                       ]
      mapM_ printAlignment ts
-}

blockWith Nothing      xs = xs
blockWith (Just (l,k)) xs = genericTake l . genericDrop (l * (k-1)) $ xs

getScores2 :: Mapping -> Lang -> Lang -> Scores
getScores2 ss a b = lliid ss M.! (a:!:b)

printAlignment :: ([Word], (Double, [[String]])) -> IO ()
printAlignment (ws,(s,[])) = do
  printf "DEBUG!\nScore: %f\nDEBUG!\n\n" s

printAlignment (ws,(s,(x:xs))) = do
  let ids = concat . intersperse " " . map (show . wordID)   $ ws
  let wds = concat . intersperse "   WORD   " . map (concat . intersperse " " . map toUtf8String . V.toList . wordWord) $ ws
  printf "IDS: %s SCORE: %f    WORDS: %s\n" ids s wds
  mapM_ putStrLn x
  putStrLn ""


tup2List (a,b) = [a,b]
tup3List (a,b,c) = [a,b,c]
tup4List (a,b,c,d) = [a,b,c,d]
