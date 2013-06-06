{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module NLP.FourWay where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Vector.Fusion.Stream.Monadic as S
import Data.Strict.Maybe
import Prelude hiding (Maybe(..))

import ADP.Fusion
import ADP.Fusion.Chr
import ADP.Fusion.Multi

data Signature m x r c e = Signature
  { bigram :: x -> c -> x
  , nil    :: e -> x
  , h      :: S.Stream m x -> m r
  }

gNWay Signature{..} w c e =
  ( w, bigram <<< w c ||| nil <<< e ... h )

score :: (Monad m, ScoreBigram c) => Signature m Int Int c e
score = Signature
  { bigram = sbigram
  , nil    = const 0
  , h      = S.foldl' max 0
  }

class ScoreBigram x where
  sbigram :: Int -> x -> Int

type PC = Maybe (Maybe Char, Char)

instance ScoreBigram (Z:.PC) where
  sbigram w (Z:.Nothing) = w + 3 -- deletion
  sbigram w (Z:.Just (Nothing,_)) = w -- word begin sentinel
  sbigram w (Z:.Just (Just p ,c)) = w + 1 -- score p/c

