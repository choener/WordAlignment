{-# LANGUAGE TypeOperators #-}

module Linguistics.FourWay.Common where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Fusion.Stream.Monadic as P hiding ((++))
import qualified Data.Vector.Unboxed as VU
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Data.Vector.Fusion.Util
import TupleTH
import Data.ByteString (ByteString)

import Data.Array.Repa.Index.Points
import Data.PrimitiveArray as PA
import Data.PrimitiveArray.Zero as PA

import ADP.Fusion
import ADP.Fusion.None
import ADP.Fusion.Empty
import ADP.Fusion.Table
import ADP.Fusion.Chr
import ADP.Fusion.Multi



data SFourWay _m _x _r c empty = SFourWay
  { loop_loop_loop_step :: _x -> ((((Z:.()):.()):.()):.c) -> _x
  , loop_loop_step_loop :: _x -> ((((Z:.()):.()):.c):.()) -> _x
  , loop_loop_step_step :: _x -> ((((Z:.()):.()):.c):.c) -> _x
  , loop_step_loop_loop :: _x -> ((((Z:.()):.c):.()):.()) -> _x
  , loop_step_loop_step :: _x -> ((((Z:.()):.c):.()):.c) -> _x
  , loop_step_step_loop :: _x -> ((((Z:.()):.c):.c):.()) -> _x
  , loop_step_step_step :: _x -> ((((Z:.()):.c):.c):.c) -> _x
  , step_loop_loop_loop :: _x -> ((((Z:.c):.()):.()):.()) -> _x
  , step_loop_loop_step :: _x -> ((((Z:.c):.()):.()):.c) -> _x
  , step_loop_step_loop :: _x -> ((((Z:.c):.()):.c):.()) -> _x
  , step_loop_step_step :: _x -> ((((Z:.c):.()):.c):.c) -> _x
  , step_step_loop_loop :: _x -> ((((Z:.c):.c):.()):.()) -> _x
  , step_step_loop_step :: _x -> ((((Z:.c):.c):.()):.c) -> _x
  , step_step_step_loop :: _x -> ((((Z:.c):.c):.c):.()) -> _x
  , step_step_step_step :: _x -> ((((Z:.c):.c):.c):.c) -> _x
  , nil_nil_nil_nil :: ((((Z:.empty):.empty):.empty):.empty) -> _x
  , h :: S.Stream _m _x -> _m _r
  }

gFourWay sFourWay {-non-terminals:-} wwww {-terminals:-} c_1 c_2 c_3 c_4 empty_1 empty_2 empty_3 empty_4 =
  (Z:.
  ( wwww ,  loop_loop_loop_step sFourWay <<< wwww % (T:!None:!None:!None:!c_4)   |||
            loop_loop_step_loop sFourWay <<< wwww % (T:!None:!None:!c_3:!None)   |||
            loop_loop_step_step sFourWay <<< wwww % (T:!None:!None:!c_3:!c_4)    |||
            loop_step_loop_loop sFourWay <<< wwww % (T:!None:!c_2:!None:!None)   |||
            loop_step_loop_step sFourWay <<< wwww % (T:!None:!c_2:!None:!c_4)    |||
            loop_step_step_loop sFourWay <<< wwww % (T:!None:!c_2:!c_3:!None)    |||
            loop_step_step_step sFourWay <<< wwww % (T:!None:!c_2:!c_3:!c_4)     |||
            step_loop_loop_loop sFourWay <<< wwww % (T:!c_1:!None:!None:!None)   |||
            step_loop_loop_step sFourWay <<< wwww % (T:!c_1:!None:!None:!c_4)    |||
            step_loop_step_loop sFourWay <<< wwww % (T:!c_1:!None:!c_3:!None)    |||
            step_loop_step_step sFourWay <<< wwww % (T:!c_1:!None:!c_3:!c_4)     |||
            step_step_loop_loop sFourWay <<< wwww % (T:!c_1:!c_2:!None:!None)    |||
            step_step_loop_step sFourWay <<< wwww % (T:!c_1:!c_2:!None:!c_4)     |||
            step_step_step_loop sFourWay <<< wwww % (T:!c_1:!c_2:!c_3:!None)     |||
            step_step_step_step sFourWay <<< wwww % (T:!c_1:!c_2:!c_3:!c_4)      |||
            nil_nil_nil_nil sFourWay <<< (T:!empty_1:!empty_2:!empty_3:!empty_4) ... h sFourWay )
  )
{-# INLINE gFourWay #-}

type PC = (Maybe Char,Char)

type Aligned = ( [ByteString], [ByteString], [ByteString], [ByteString] )

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
{-# INLINE (<**) #-}

fillTable4 (Z:.(MTbl _ tbl, f)) = do
  let (_,Z:.PointL(0:.n1):.PointL(0:.n2):.PointL(0:.n3):.PointL(0:.n4)) = boundsM tbl
  forM_ [0 .. n1] $ \k1 -> forM_ [0 .. n2] $ \k2 -> forM_ [0 .. n3] $ \k3 -> forM_ [0..n4] $ \k4 -> do
    (f $ Z:.pointL 0 k1:.pointL 0 k2:.pointL 0 k3:.pointL 0 k4) >>= writeM tbl (Z:.pointL 0 k1:.pointL 0 k2:.pointL 0 k3:.pointL 0 k4)
{-# INLINE fillTable4 #-}

