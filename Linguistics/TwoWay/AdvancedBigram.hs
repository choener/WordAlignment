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

bigram :: Monad m => SigBigramGrammar m Double Double (Maybe InternedMultiChar, InternedMultiChar) () InternedMultiChar ()
bigram = SigBigramGrammar
  { biBi     = \ x (Z:.a :.b ) -> x -- TODO
  , biGext   = \ x (Z:.a :.()) -> x -- TODO
  , biGopen  = \ x (Z:.a :.()) -> x -- TODO
  , biUni    = \ x (Z:.a :.b ) -> x -- TODO
  , gextBi   = \ x (Z:.():.b ) -> x -- TODO
  , gextUni  = \ x (Z:.():.b ) -> x -- TODO
  , gopenBi  = \ x (Z:.():.b ) -> x -- TODO
  , gopenUni = \ x (Z:.():.b ) -> x -- TODO
  , nilNil   = \   (Z:.():.()) -> 0
  , uniBi    = \ x (Z:.a :.b ) -> x -- TODO
  , uniGext  = \ x (Z:.a :.()) -> x -- TODO
  , uniGopen = \ x (Z:.a :.()) -> x -- TODO
  , h        = SM.foldl' max (-999999)
  }
{-# INLINE bigram #-}

type IMS = [InternedMultiChar]

pretty :: Monad m => SigBigramGrammar m [IMS] (SM.Stream m [IMS]) (Maybe InternedMultiChar, InternedMultiChar) () InternedMultiChar ()
pretty = SigBigramGrammar
  { biBi     = \ [x,y] (Z:.(_,a) :.(_,b) ) -> [a  :x,b  :y]
  , biGext   = \ [x,y] (Z:.(_,a) :.()    ) -> [a  :x,"-":y]
  , biGopen  = \ [x,y] (Z:.(_,a) :.()    ) -> [a  :x,"-":y]
  , biUni    = \ [x,y] (Z:.(_,a) :.b     ) -> [a  :x,b  :y]
  , gextBi   = \ [x,y] (Z:.()    :.(_,b) ) -> ["-":x,b  :y]
  , gextUni  = \ [x,y] (Z:.()    :.b     ) -> ["-":x,b  :y]
  , gopenBi  = \ [x,y] (Z:.()    :.(_,b) ) -> ["-":x,b  :y]
  , gopenUni = \ [x,y] (Z:.()    :.b     ) -> ["-":x,b  :y]
  , nilNil   = \       (Z:.()    :.()    ) -> [[],[]]
  , uniBi    = \ [x,y] (Z:.a     :.(_,b) ) -> [a  :x,b  :y]
  , uniGext  = \ [x,y] (Z:.a     :.()    ) -> [a  :x,"-":y]
  , uniGopen = \ [x,y] (Z:.a     :.()    ) -> [a  :x,"-":y]
  , h        = return . id
  }

type Tbl = Unboxed (Z:.PointL:.PointL) Double

forward :: V.Vector InternedMultiChar -> V.Vector InternedMultiChar -> ST s (Tbl,Tbl,Tbl)
forward as bs = do
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
  let tDD = undefined `asTypeOf` tDM
  fillTable $ gBigramGrammar bigram tDD tDM tMD tMM aP bP Empty Empty aa bb
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

backtrack :: V.Vector InternedMultiChar -> V.Vector InternedMultiChar -> (Tbl,Tbl,Tbl) -> [[IMS]]
backtrack as bs (tDM',tMD',tMM') = unId . SM.toList . unId . fMM $ Z:.pointL 0 aL:.pointL 0 bL where
  aL = V.length as
  bL = V.length bs
  aa = chr as
  bb = chr bs
  aP = chrLeft as
  bP = chrLeft bs
  tDM = btTblD (Z:.EmptyOk:.EmptyOk) tDM' fDM
  tMD = btTblD (Z:.EmptyOk:.EmptyOk) tMD' fMD
  tMM = btTblD (Z:.EmptyOk:.EmptyOk) tMM' fMM
  tDD = undefined `asTypeOf` tDM
  ((_,fDM),(_,fMD),(_,fMM)) = gBigramGrammar (bigram <** pretty) tDD tDM tMD tMM aP bP Empty Empty aa bb
{-# NOINLINE backtrack #-}

runBigram :: Int -> IMS -> IMS -> (Double,[[IMS]])
runBigram k as bs = (tMM PA.! (Z:.pointL 0 aL:.pointL 0 bL), take k b) where
  aa = V.fromList as
  bb = V.fromList bs
  aL = V.length aa
  bL = V.length bb
  (tDM,tMD,tMM) = runST $ forward aa bb
  b = backtrack aa bb (tDM,tMD,tMM)
{-# NOINLINE runBigram #-}

