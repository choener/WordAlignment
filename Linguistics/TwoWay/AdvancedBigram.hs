{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Linguistics.TwoWay.AdvancedBigram where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List
import           Data.Vector.Fusion.Util
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import           Text.Printf
import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.HashTable.IO as H
import           Data.Strict.Tuple (Pair (..))

import           ADP.Fusion
import           Data.Array.Repa.Index
import           Data.Array.Repa.Index.Points
import           Data.PrimitiveArray as PA
import           Data.PrimitiveArray.Zero as PA
import           FormalLanguage.CFG
import           FormalLanguage.GrammarProduct
import           FormalLanguage.GrammarProduct.QQ
import           NLP.Alphabet.MultiChar

import           Linguistics.Bigram


[grammarProductF|Linguistics/TwoWay/AdvancedBigram.gra|]

makeAlgebraProductH ['h] ''SigBigramGrammar

bigram :: Monad m => Double -> Double -> Double -> Scores -> SigBigramGrammar m Double Double (Maybe InternedMultiChar, InternedMultiChar) () InternedMultiChar ()
bigram gapopen gapextend defaultScore scores = SigBigramGrammar
  { biBi     = \ x (Z:.a :.b ) -> x + if | (Nothing,_ ) <- a, (Nothing,_ ) <- b -> 0                -- word init
                                         | (Just ap,a') <- a, (Just bp,b') <- b -> lkup ap a' bp b' -- bigram alignment
                                         | otherwise                            -> -999999
  , biGext   = \ x (Z:.a :.()) -> x + gapextend
  , biGopen  = \ x (Z:.a :.()) -> x + gapopen
  , biUni    = \ x (Z:.a :.b ) -> x + if | (Just ap,a') <- a -> lkup ap a' "-" b
                                         | otherwise         -> -999999
  , gextBi   = \ x (Z:.():.b ) -> x + gapextend
  , gopenBi  = \ x (Z:.():.b ) -> x + gapopen
  , gopenUni = \ x (Z:.():.b ) -> x - 999999 -- TODO do we want to keep this rule?
  , nilNil   = \   (Z:.():.()) -> 0
  , uniBi    = \ x (Z:.a :.b ) -> x + if | (Just bp,b') <- b -> lkup "-" a bp b'
                                         | otherwise         -> -999999
  , uniGopen = \ x (Z:.a :.()) -> x - 999999 -- TODO do we want to keep this rule?
  , h        = SM.foldl' max (-999999)
  } where
    lkup pa a pb b = maybe defaultScore id . unsafePerformIO $ H.lookup scores (Bigram pa a :!: Bigram pb b)
{-# INLINE bigram #-}

type IMS = [InternedMultiChar]

pretty :: Monad m => SigBigramGrammar m [IMS] (SM.Stream m [IMS]) (Maybe InternedMultiChar, InternedMultiChar) () InternedMultiChar ()
pretty = SigBigramGrammar
  { biBi     = \ [x,y] (Z:.(_,a) :.(_,b) ) -> [a  :x,b  :y]
  , biGext   = \ [x,y] (Z:.(_,a) :.()    ) -> [a  :x,"-":y]
  , biGopen  = \ [x,y] (Z:.(_,a) :.()    ) -> [a  :x,"-":y]
  , biUni    = \ [x,y] (Z:.(_,a) :.b     ) -> [a  :x,b  :y]
  , gextBi   = \ [x,y] (Z:.()    :.(_,b) ) -> ["-":x,b  :y]
  , gopenBi  = \ [x,y] (Z:.()    :.(_,b) ) -> ["-":x,b  :y]
  , gopenUni = \ [x,y] (Z:.()    :.b     ) -> ["-":x,b  :y]
  , nilNil   = \       (Z:.()    :.()    ) -> [[],[]]
  , uniBi    = \ [x,y] (Z:.a     :.(_,b) ) -> [a  :x,b  :y]
  , uniGopen = \ [x,y] (Z:.a     :.()    ) -> [a  :x,"-":y]
  , h        = return . id
  }

type Tbl = Unboxed (Z:.PointL:.PointL) Double

forward :: Double -> Double -> Double -> Scores -> V.Vector InternedMultiChar -> V.Vector InternedMultiChar -> ST s (Tbl,Tbl,Tbl)
forward gapOpen gapExtend defaultScore scores as bs = do
  let aL = V.length as
  let bL = V.length bs
  let aa = chr as
  let bb = chr bs
  let aP = chrLeft as
  let bP = chrLeft bs
  !tDM' <- PA.newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 aL:.pointL 0 bL) (-999999)
  !tMD' <- PA.newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 aL:.pointL 0 bL) (-999999)
  !tMM' <- PA.newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 aL:.pointL 0 bL) (-999999)
  let tDM = mTblD (Z:.EmptyOk:.EmptyOk) tDM'
  let tMD = mTblD (Z:.EmptyOk:.EmptyOk) tMD'
  let tMM = mTblD (Z:.EmptyOk:.EmptyOk) tMM'
  fillTable $ gBigramGrammar (bigram gapOpen gapExtend defaultScore scores) tDM tMD tMM aP bP Empty Empty aa bb
  ss <- PA.freeze tDM'
  tt <- PA.freeze tMD'
  uu <- PA.freeze tMM'
  return (ss,tt,uu)
{-# NOINLINE forward #-}

fillTable ( (MTbl _ tDM,fDM), (MTbl _ tMD,fMD), (MTbl _ tMM,fMM) ) = do
  let (_,Z:.PointL (_:.aL):.PointL (_:.bL)) = boundsM tMM
  forM_ [0 .. aL] $ \a -> forM_ [0 .. bL] $ \b -> do
    let ix = Z:.pointL 0 a:.pointL 0 b
    (fDM ix) >>= PA.writeM tDM ix
    (fMD ix) >>= PA.writeM tMD ix
    (fMM ix) >>= PA.writeM tMM ix
{-# INLINE fillTable #-}

backtrack :: Double -> Double -> Double -> Scores -> V.Vector InternedMultiChar -> V.Vector InternedMultiChar -> (Tbl,Tbl,Tbl) -> [[IMS]]
backtrack gapOpen gapExtend defaultScore scores as bs (tDM',tMD',tMM') = unId . SM.toList . unId . fMM $ Z:.pointL 0 aL:.pointL 0 bL where
  aL = V.length as
  bL = V.length bs
  aa = chr as
  bb = chr bs
  aP = chrLeft as
  bP = chrLeft bs
  tDM = btTblD (Z:.EmptyOk:.EmptyOk) tDM' fDM
  tMD = btTblD (Z:.EmptyOk:.EmptyOk) tMD' fMD
  tMM = btTblD (Z:.EmptyOk:.EmptyOk) tMM' fMM
  ((_,fDM),(_,fMD),(_,fMM)) = gBigramGrammar (bigram gapOpen gapExtend defaultScore scores <** pretty) tDM tMD tMM aP bP Empty Empty aa bb
{-# NOINLINE backtrack #-}

runBigram :: Double -> Double -> Double -> Scores -> Int -> V.Vector InternedMultiChar -> V.Vector InternedMultiChar -> (Double,[[IMS]])
runBigram gapOpen gapExtend defaultScore scores k as bs = (tMM PA.! (Z:.pointL 0 aL:.pointL 0 bL), take k b) where
  aL = V.length as
  bL = V.length bs
  (tDM,tMD,tMM) = runST $ forward gapOpen gapExtend defaultScore scores as bs
  b = backtrack gapOpen gapExtend defaultScore scores as bs (tDM,tMD,tMM)
{-# NOINLINE runBigram #-}

