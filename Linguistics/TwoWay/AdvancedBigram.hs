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
import           Data.Strict.Tuple (Pair (..))
import           Data.Stringable
import           Data.Vector.Fusion.Util
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.HashTable.IO as H
import qualified Data.Vector as V
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Printf

import           ADP.Fusion
import           Data.Array.Repa.Index
import           Data.Array.Repa.Index.Points
import           Data.PrimitiveArray as PA hiding (map)
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
  { biBi     = \ x (Z:.a :.b ) -> x + scoreBiBi defaultScore scores a b
  , biGext   = \ x (Z:.a :.()) -> x + gapextend
  , biGopen  = \ x (Z:.a :.()) -> x + gapopen
  , biUni    = \ x (Z:.a :.b ) -> x + scoreBiUni defaultScore scores a b
  , gextBi   = \ x (Z:.():.b ) -> x + gapextend
  , gopenBi  = \ x (Z:.():.b ) -> x + gapopen
  , gopenUni = \ x (Z:.():.b ) -> x - 999999 -- TODO do we want to keep this rule?
  , nilNil   = \   (Z:.():.()) -> 0
  , uniBi    = \ x (Z:.a :.b ) -> x + scoreUniBi defaultScore scores a b
  , uniGopen = \ x (Z:.a :.()) -> x - 999999 -- TODO do we want to keep this rule?
  , h        = SM.foldl' max (-999999)
  }
{-# INLINE bigram #-}

lkup defaultScore scores pa a pb b = maybe defaultScore id . unsafePerformIO $ H.lookup scores (Bigram pa a :!: Bigram pb b)
{-# INLINE lkup #-}

scoreBiBi defaultScore scores a b
  | (Nothing,_  ) <- a, (Nothing,_  ) <- b = 0
  | (Just ap,a' ) <- a, (Just bp,b' ) <- b = lkup defaultScore scores ap a' bp b'
  | (_      ,"$") <- a, (_      ,"^") <- b = 0
  | otherwise                            = -999999
{-# INLINE scoreBiBi #-}

scoreBiUni defaultScore scores a b
  | (Just ap,a' ) <- a = lkup defaultScore scores ap a' "-" b
  | (_      ,"$") <- a, "^" <- b = 0
  | otherwise         = -999999
{-# INLINE scoreBiUni #-}

scoreUniBi defaultScore scores a b
  | (Just bp,b') <- b = lkup defaultScore scores "-" a bp b'
  | otherwise         = -999999
{-# INLINE scoreUniBi #-}

type IMS = [InternedMultiChar]

fss = fromString . printf "%5.2f"
{-# iNLINE fss #-}

pretty :: Monad m => Double -> Double -> Double -> Scores -> SigBigramGrammar m [IMS] (SM.Stream m [IMS]) (Maybe InternedMultiChar, InternedMultiChar) () InternedMultiChar ()
pretty gapOpen gapExtend defaultScore scores = SigBigramGrammar
  { biBi     = \ [x,y,s] (Z:.a :.b ) -> [snd a :x,snd b :y,(fss $ scoreBiBi defaultScore scores a b)  : s]
  , biGext   = \ [x,y,s] (Z:.a :.()) -> [snd a :x,"-"   :y,(fss $ gapExtend)                          : s]
  , biGopen  = \ [x,y,s] (Z:.a :.()) -> [snd a :x,"-"   :y,(fss $ gapOpen)                            : s]
  , biUni    = \ [x,y,s] (Z:.a :.b ) -> [snd a :x,b     :y,(fss $ scoreBiUni defaultScore scores a b) : s]
  , gextBi   = \ [x,y,s] (Z:.():.b ) -> ["-"   :x,snd b :y,(fss $ gapExtend)                          : s]
  , gopenBi  = \ [x,y,s] (Z:.():.b ) -> ["-"   :x,snd b :y,(fss $ gapOpen)                            : s]
  , gopenUni = \ [x,y,s] (Z:.():.b ) -> ["-"   :x,b     :y,(fss $ -999999)                            : s]
  , nilNil   = \         (Z:.():.()) -> [[],[],[]]
  , uniBi    = \ [x,y,s] (Z:.a :.b ) -> [a     :x,snd b :y,(fss $ scoreUniBi defaultScore scores a b) : s]
  , uniGopen = \ [x,y,s] (Z:.a :.()) -> [a     :x,"-"   :y,(fss $ -999999)                            : s]
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
backtrack gapOpen gapExtend defaultScore scores as bs (tDM',tMD',tMM') = unId . SM.toList . unId . f $ Z:.pointL 0 aL:.pointL 0 bL where
  aL = V.length as
  bL = V.length bs
  aa = chr as
  bb = chr bs
  aP = chrLeft as
  bP = chrLeft bs
  tDM = btTblD (Z:.EmptyOk:.EmptyOk) tDM' fDM
  tMD = btTblD (Z:.EmptyOk:.EmptyOk) tMD' fMD
  tMM = btTblD (Z:.EmptyOk:.EmptyOk) tMM' fMM
  ix = (Z:.pointL 0 aL:.pointL 0 bL)
  mx = maximum $ map (PA.!ix) [tDM',tMD',tMM']
  f = if | mx==tDM' PA.! ix -> fDM
         | mx==tMD' PA.! ix -> fMD
         | mx==tMM' PA.! ix -> fMM
  ((_,fDM),(_,fMD),(_,fMM)) = gBigramGrammar (bigram gapOpen gapExtend defaultScore scores <** pretty gapOpen gapExtend defaultScore scores) tDM tMD tMM aP bP Empty Empty aa bb
{-# NOINLINE backtrack #-}

runBigram :: Double -> Double -> Double -> Scores -> Int -> V.Vector InternedMultiChar -> V.Vector InternedMultiChar -> (Double,[[IMS]])
runBigram gapOpen gapExtend defaultScore scores k as bs = (maximum $ map (PA.!ix) [tDM,tMD,tMM] , take k b) where
  ix = (Z:.pointL 0 aL:.pointL 0 bL)
  aL = V.length as
  bL = V.length bs
  (tDM,tMD,tMM) = runST $ forward gapOpen gapExtend defaultScore scores as bs
  b = backtrack gapOpen gapExtend defaultScore scores as bs (tDM,tMD,tMM)
{-# NOINLINE runBigram #-}

