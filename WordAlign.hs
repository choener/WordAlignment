{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Console.CmdArgs
import Data.List (tails)
import Text.Printf
import Control.Arrow
import Control.Parallel.Strategies
import Data.Vector (fromList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Map as M
import Data.List (intersperse)
import qualified Data.Vector as V

import Linguistics.TwoWay
import Linguistics.FourWay
import Linguistics.Tools

data Config
  = TwoWay
    { scoreFile :: String
    , defaultScore :: Double
    }
  | FourWay
    { scoreFile :: String
    , defaultScore :: Double
    }
  deriving (Show,Data,Typeable)

twoway = TwoWay
  { scoreFile = "" &= help "the file to read the scores from"
  , defaultScore = (-42) &= help "score to use for unknown bigram matches"
  }

fourway = FourWay
  {
  }

main = do
  o <- cmdArgs $ modes [twoway,fourway]
  ss <- TL.readFile (scoreFile o) >>= return . parseScoreFile (defaultScore o) . TL.unlines . take 100000 . TL.lines
--  mapM_ print $ concatMap (M.toList . scores) $ M.elems $ ss
  -- ls <- fmap (map (fromList . T.words) . T.lines) $ T.getContents
  ls <- TL.getContents >>= return . wordParser . TL.lines
  case o of
    TwoWay{..} -> do
      let ts = [ ([a,b],second (map tup2List) $ nWay2 (getScores2 ss (wordLang a) (wordLang b)) (wordWord a) (wordWord b))
               | (a:as) <- tails ls, b <- as ]
      mapM_ printAlignment ts
{-
    FourWay{..} -> do
      let ts = [ second (map tup4List) $ nWay4 a b c d | (a:as) <- tails ls
                                                       , (b:bs) <- tails as
                                                       , (c:cs) <- tails bs
                                                       , d      <- cs
                                                       ]
      mapM_ printAlignment ts
-}

getScores2 ss a b = ss M.! (a,b)

printAlignment (ws,(s,[])) = do
  printf "DEBUG!\nScore: %f\nDEBUG!\n\n" s

printAlignment (ws,(s,(x:xs))) = do
  --mapM_ (\w -> (mapM_ (\x -> T.putStr x >> putStr " ") . V.toList . wordWord $ w) >> putStrLn "") ws
  mapM_ print ws
  printf "Score: %f\n" s
  mapM_ putStrLn x
  putStrLn ""


tup2List (a,b) = [a,b]
tup3List (a,b,c) = [a,b,c]
tup4List (a,b,c,d) = [a,b,c,d]
