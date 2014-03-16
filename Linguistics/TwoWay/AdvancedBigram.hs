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
  !s' <- PA.newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 aL:.pointL 0 bL) (-999999)
  !t' <- PA.newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 aL:.pointL 0 bL) (-999999)
  !u' <- PA.newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 aL:.pointL 0 bL) (-999999)
  let s = mTblD (Z:.EmptyOk:.EmptyOk) s'
  let t = mTblD (Z:.EmptyOk:.EmptyOk) t'
  let u = mTblD (Z:.EmptyOk:.EmptyOk) u'
  fillTable $ gBigramGrammar bigram s t u u aP bP Empty Empty aa bb
  ss <- PA.freeze s'
  tt <- PA.freeze t'
  uu <- PA.freeze u'
  return (ss,tt,uu)
{-# NOINLINE forward #-}

fillTable ( (MTbl _ s,f), (MTbl _ t,g), (MTbl _ u,h) ) = do
  let (_,Z:.PointL (_:.aL):.PointL (_:.bL)) = boundsM t
  forM_ [0 .. aL] $ \a -> forM_ [0 .. bL] $ \b -> do
    let ix = Z:.pointL 0 a:.pointL 0 b
    (f ix) >>= PA.writeM s ix
    (g ix) >>= PA.writeM t ix
    (h ix) >>= PA.writeM u ix
{-# INLINE fillTable #-}

backtrack :: V.Vector InternedMultiChar -> V.Vector InternedMultiChar -> (Tbl,Tbl,Tbl) -> [[IMS]]
backtrack as bs (s',t',u') = unId . SM.toList . unId . h $ Z:.pointL 0 aL:.pointL 0 bL where
  aL = V.length as
  bL = V.length bs
  aa = chr as
  bb = chr bs
  aP = chrLeft as
  bP = chrLeft bs
  s = btTblD (Z:.EmptyOk:.EmptyOk) s' f
  t = btTblD (Z:.EmptyOk:.EmptyOk) t' g
  u = btTblD (Z:.EmptyOk:.EmptyOk) u' h
  ((_,f),(_,g),(_,h)) = gBigramGrammar (bigram <** pretty) s t u u aP bP Empty Empty aa bb
{-# NOINLINE backtrack #-}

runBigram :: Int -> IMS -> IMS -> (Double,[[IMS]])
runBigram k as bs = (u PA.! (Z:.pointL 0 aL:.pointL 0 bL), take k b) where
  aa = V.fromList as
  bb = V.fromList bs
  aL = V.length aa
  bL = V.length bb
  (s,t,u) = runST $ forward aa bb
  b = backtrack aa bb (s,t,u)
{-# NOINLINE runBigram #-}

