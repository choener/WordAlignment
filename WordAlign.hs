{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import System.Console.CmdArgs
import Data.List (tails)
import Text.Printf
import Control.Arrow
import Control.Parallel.Strategies
import Data.Vector.Unboxed (fromList)

import Linguistics.TwoWay
import Linguistics.FourWay



data Config
  = TwoWay
  | FourWay
  deriving (Show,Data,Typeable)

twoway = TwoWay
  {
  }

fourway = FourWay
  {
  }

main = do
  o <- cmdArgs $ modes [twoway]
  ls <- fmap (map fromList . lines) $ getContents
  case o of
    TwoWay{..} -> do
      let ts = [ second (map tup2List) $ nWay2 a b | (a:as) <- tails ls, b <- as ] -- `using` parBuffer 100 (evalTuple2 rdeepseq r0)
      mapM_ printAlignment ts
    FourWay{..} -> do
      let ts = [ second (map tup4List) $ nWay4 a b c d | (a:as) <- tails ls
                                                       , (b:bs) <- tails as
                                                       , (c:cs) <- tails bs
                                                       , d      <- cs
                                                       ] -- `using` parBuffer 100 (evalTuple2 rdeepseq r0)
      mapM_ printAlignment ts


printAlignment (s,(x:xs)) = do
  printf "Score: %d\n" s
  mapM_ putStrLn x
  putStrLn ""


tup2List (a,b) = [a,b]
tup3List (a,b,c) = [a,b,c]
tup4List (a,b,c,d) = [a,b,c,d]
