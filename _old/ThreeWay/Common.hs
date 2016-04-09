{-# LANGUAGE TypeOperators #-}

module Linguistics.ThreeWay.Common where

import Data.Array.Repa.Index
import Data.Vector.Fusion.Stream.Monadic (Stream (..))
import Data.ByteString (ByteString)
import Control.Monad
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.ByteString (ByteString)
import Data.Vector.Fusion.Stream.Monadic (Stream (..))
import qualified Data.Vector.Fusion.Stream.Monadic as S

import ADP.Fusion
import ADP.Fusion.Multi
import Data.PrimitiveArray (boundsM, writeM)
import Data.Array.Repa.Index.Points
import ADP.Fusion
import ADP.Fusion.Multi



data SThreeWay _m _x _r c empty = SThreeWay
  { loop_loop_step :: _x -> (((Z:.()):.()):.c) -> _x
  , loop_step_loop :: _x -> (((Z:.()):.c):.()) -> _x
  , loop_step_step :: _x -> (((Z:.()):.c):.c) -> _x
  , step_loop_loop :: _x -> (((Z:.c):.()):.()) -> _x
  , step_loop_step :: _x -> (((Z:.c):.()):.c) -> _x
  , step_step_loop :: _x -> (((Z:.c):.c):.()) -> _x
  , step_step_step :: _x -> (((Z:.c):.c):.c) -> _x
  , nil_nil_nil :: (((Z:.empty):.empty):.empty) -> _x
  , h :: Stream _m _x -> _m _r
  }

gThreeWay sThreeWay {-non-terminals:-} www {-terminals:-} c_1 c_2 c_3 empty_1 empty_2 empty_3 =
  (Z:.
  ( www , loop_loop_step sThreeWay <<< www % (T:!None:!None:!c_3)     |||
          loop_step_loop sThreeWay <<< www % (T:!None:!c_2:!None)     |||
          loop_step_step sThreeWay <<< www % (T:!None:!c_2:!c_3)      |||
          step_loop_loop sThreeWay <<< www % (T:!c_1:!None:!None)     |||
          step_loop_step sThreeWay <<< www % (T:!c_1:!None:!c_3)      |||
          step_step_loop sThreeWay <<< www % (T:!c_1:!c_2:!None)      |||
          step_step_step sThreeWay <<< www % (T:!c_1:!c_2:!c_3)       |||
          nil_nil_nil    sThreeWay <<< (T:!empty_1:!empty_2:!empty_3) ... h sThreeWay )
  )
{-# INLINE gThreeWay #-}

type Aligned = ( [ByteString], [ByteString], [ByteString] )

(<**) f s = SThreeWay l_l_s l_s_l l_s_s s_l_l s_l_s s_s_l s_s_s n_n_n h where
  SThreeWay llsf lslf lssf sllf slsf sslf sssf nnnf hf = f
  SThreeWay llss lsls lsss slls slss ssls ssss nnns hs = s
  l_l_s = go llsf llss
  l_s_l = go lslf lsls
  l_s_s = go lssf lsss
  s_l_l = go sllf slls
  s_l_s = go slsf slss
  s_s_l = go sslf ssls
  s_s_s = go sssf ssss
  n_n_n e = (nnnf e, return $ S.singleton $ nnns e)
  h xs = do
    hfs <- hf $ S.map fst xs
    let phfs = S.concatMapM snd . S.filter ((hfs==) . fst) $ xs
    hs phfs
  go funL funR (x,ys) c = (funL x c, ys >>= return . S.map (\y -> funR y c))
{-# INLINE (<**) #-}

fillTable3 (Z:.(MTbl _ tbl, f)) = do
  let (_,Z:.PointL(0:.n1):.PointL(0:.n2):.PointL(0:.n3)) = boundsM tbl
  forM_ [0 .. n1] $ \k1 -> forM_ [0 .. n2] $ \k2 -> forM_ [0 .. n3] $ \k3 -> do
    (f $ Z:.pointL 0 k1:.pointL 0 k2:.pointL 0 k3) >>= writeM tbl (Z:.pointL 0 k1:.pointL 0 k2:.pointL 0 k3)
{-# INLINE fillTable3 #-}

