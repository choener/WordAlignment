{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Linguistics.FourWay where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Fusion.Stream.Monadic as P hiding ((++))
import qualified Data.Vector.Unboxed as VU
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Data.Vector.Fusion.Util
import TupleTH

import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import ADP.Fusion
import ADP.Fusion.None
import ADP.Fusion.Empty
import ADP.Fusion.Table
import ADP.Fusion.Chr
import ADP.Fusion.Multi

import Linguistics.FourWay.Common



sScore :: Monad m => SFourWay m Int Int (Maybe Char,Char) ()
sScore = SFourWay
  { loop_loop_loop_step = loopstep
  , loop_loop_step_loop = loopstep
  , loop_loop_step_step = loopstep
  , loop_step_loop_loop = loopstep
  , loop_step_loop_step = loopstep
  , loop_step_step_loop = loopstep
  , loop_step_step_step = loopstep
  , step_loop_loop_loop = loopstep
  , step_loop_loop_step = loopstep
  , step_loop_step_loop = loopstep
  , step_loop_step_step = loopstep
  , step_step_loop_loop = loopstep
  , step_step_loop_step = loopstep
  , step_step_step_loop = loopstep
  , step_step_step_step = loopstep
  , nil_nil_nil_nil = const 0
  , h         = S.foldl' max 0
  }
{-# INLINE sScore #-}

loopstep w (Z:.a:.b:.c:.d) = w + (sum [pairScore a b, pairScore a c, pairScore a d, pairScore b c, pairScore b d, pairScore c d])
{-# INLINE loopstep #-}

type PC = (Maybe Char,Char)

class PairScore l r where
  pairScore :: l -> r -> Int

instance PairScore PC PC where
  pairScore (ma,a) (mb,b) = if a==b then 1 else 0

instance PairScore PC () where
  pairScore _ _ = 0

instance PairScore () PC where
  pairScore _ _ = 0

instance PairScore () () where
  pairScore _ _ = 0

sAlign4 :: Monad m => SFourWay m (String,String,String,String) (S.Stream m (String,String,String,String)) (Maybe Char,Char) ()
sAlign4 = SFourWay
  { loop_loop_loop_step = alignfake
  , loop_loop_step_loop = alignfake
  , loop_loop_step_step = alignfake
  , loop_step_loop_loop = alignfake
  , loop_step_loop_step = alignfake
  , loop_step_step_loop = alignfake
  , loop_step_step_step = alignfake
  , step_loop_loop_loop = alignfake
  , step_loop_loop_step = alignfake
  , step_loop_step_loop = alignfake
  , step_loop_step_step = alignfake
  , step_step_loop_loop = alignfake
  , step_step_loop_step = alignfake
  , step_step_step_loop = alignfake
  , step_step_step_step = alignfake
  , nil_nil_nil_nil = const ("","","","")
  , h         = return . id
  }
{-# NOINLINE sAlign4 #-}

alignfake (w1,w2,w3,w4) (Z:.a:.b:.c:.d) = (w1++addAlign a, w2++addAlign b, w3++addAlign c, w4++addAlign d)

class AddAlign x where
  addAlign :: x -> String

instance AddAlign () where
  addAlign () = "-"

instance AddAlign PC where
  addAlign (_,a) = [a]

nWay4 i1 i2 i3 i4 = (ws ! (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3:.pointL 0 n4), bt) where
  ws = unsafePerformIO (nWay4Fill i1 i2 i3 i4)
  n1 = VU.length i1
  n2 = VU.length i2
  n3 = VU.length i3
  n4 = VU.length i4
  bt = [] -- backtrack4 i1 i2 i3 i4 ws
{-# NOINLINE nWay4 #-}

nWay4Fill
  :: VU.Vector Char
  -> VU.Vector Char
  -> VU.Vector Char
  -> VU.Vector Char
  -> IO (PA.Unboxed (Z:.PointL:.PointL:.PointL:.PointL) Int)
nWay4Fill i1 i2 i3 i4 = do
  let n1 = VU.length i1
  let n2 = VU.length i2
  let n3 = VU.length i3
  let n4 = VU.length i4
  !t' <- newWithM (Z:.pointL 0 0:.pointL 0 0:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 n1:.pointL 0 n2:.pointL 0 n3:.pointL 0 n4) 0
  let w = mTbl (Z:.EmptyT:.EmptyT:.EmptyT:.EmptyT) t'
  fillTable4 $ gFourWay sScore w (chrLeft i1) (chrLeft i2) (chrLeft i3) (chrLeft i4) Empty Empty Empty Empty
  freeze t'
{-# NOINLINE nWay4Fill #-}

fillTable4 (Z:.(MTbl _ tbl, f)) = do
  let (_,Z:.PointL(0:.n1):.PointL(0:.n2):.PointL(0:.n3):.PointL(0:.n4)) = boundsM tbl
  forM_ [0 .. n1] $ \k1 -> forM_ [0 .. n2] $ \k2 -> forM_ [0 .. n3] $ \k3 -> forM_ [0 .. n4] $ \k4 -> do
    (f $ Z:.pointL 0 k1:.pointL 0 k2:.pointL 0 k3:.pointL 0 k4) >>= writeM tbl (Z:.pointL 0 k1:.pointL 0 k2:.pointL 0 k3:.pointL 0 k4)
{-# INLINE fillTable4 #-}

{-
backtrack4 (i1 :: VU.Vector Char) (i2 :: VU.Vector Char) (i3 :: VU.Vector Char) (i4 :: VU.Vector Char) tbl
  = unId . P.toList . unId $ g $ Z:.pointL 0 n1 :.pointL 0 n2 :.pointL 0 n3 :.pointL 0 n4
  where
    n1 = VU.length i1
    n2 = VU.length i2
    n3 = VU.length i3
    n4 = VU.length i4
    w :: DefBtTbl Id (Z:.PointL:.PointL:.PointL:.PointL) Int (String,String,String,String)
    w = btTbl (Z:.EmptyT:.EmptyT:.EmptyT:.EmptyT) tbl g -- (g :: (Z:.PointL:.PointL:.PointL:.PointL) -> Id (S.Stream Id (String,String,String,String)))
    (Z:.(_,g)) = gFourWay (sScore <** sAlign4) w (chrLeft i1) (chrLeft i2) (chrLeft i3) (chrLeft i4) Empty Empty Empty Empty
{-# NOINLINE backtrack4 #-}

(<**) f s = SFourWay llls llsl llss lsll lsls lssl lsss slll slls slsl slss ssll ssls sssl ssss nnnn h where
  SFourWay lllsf llslf llssf lsllf lslsf lsslf lsssf slllf sllsf slslf slssf ssllf sslsf ssslf ssssf nnnnf hf = f
  SFourWay lllss llsls llsss lslls lslss lssls lssss sllls sllss slsls slsss sslls sslss sssls sssss nnnns hs = s
  llls = go lllsf lllss
  llsl = go llslf llsls
  llss = go llssf llsss
  lsll = go lsllf lslls
  lsls = go lslsf lslss
  lssl = go lsslf lssls
  lsss = go lsssf lssss
  slll = go slllf sllls
  slls = go sllsf sllss
  slsl = go slslf slsls
  slss = go slssf slsss
  ssll = go ssllf sslls
  ssls = go sslsf sslss
  sssl = go ssslf sssls
  ssss = go ssssf sssss
  nnnn e = (nnnnf e, return $ S.singleton $ nnnns e)
  h xs = do
    hfs <- hf $ S.map fst xs
    let phfs = S.concatMapM snd . S.filter ((hfs==) . fst) $ xs
    hs phfs
  go funL funR (x,ys) c = (funL x c, ys >>= return . S.map (\y -> funR y c))
-}
