{- LANGUAGE TypeOperators #-}

module Linguistics.WordAlignment.TwoWay.Common where

import           Control.Monad
import           Data.ByteString (ByteString)
import           Data.Vector.Fusion.Stream.Monadic (Stream (..))
import qualified Data.Vector.Fusion.Stream.Monadic as S

import           ADP.Fusion
import           Data.PrimitiveArray
import           NLP.Alphabet.MultiChar
import           FormalLanguage







{-
-- * Signature

data STwoWay _m _x _r c empty = STwoWay
  { loop_step :: _x -> (Z:.():.c) -> _x
  , step_loop :: _x -> (Z:.c:.()) -> _x
  , step_step :: _x -> (Z:.c:.c)  -> _x
  , nil_nil :: (Z:.empty:.empty)  -> _x
  , h :: Stream _m _x -> _m _r
  }



-- * Grammar

gTwoWay sTwoWay {-non-terminals:-} ww {-terminals:-} c1 c2 empty1 empty2 =
  (Z:.
  ( ww ,  loop_step sTwoWay  <<< ww % (M:>None:>c2)  |||
          step_loop sTwoWay  <<< ww % (M:>c1:>None)  |||
          step_step sTwoWay  <<< ww % (M:>c1:>c2)    |||
          nil_nil   sTwoWay  <<< (M:>empty1:>empty2) ... h sTwoWay )
  )
{-# INLINE gTwoWay #-}

-- | Type of backtracked results

type Aligned = [[InternedMultiChar]]

-- | Algebra product operation

(<**) f s = STwoWay l_s s_l s_s n_n h where
  STwoWay lsf slf ssf nnf hf = f -- (emptyF,leftF,rightF,pairF,splitF,hF) = f
  STwoWay lss sls sss nns hs = s -- (emptyS,leftS,rightS,pairS,splitS,hS) = s
  l_s = go lsf lss
  s_l = go slf sls
  s_s = go ssf sss
  n_n e = (nnf e, return $ S.singleton $ nns e)
  h xs = do
    hfs <- hf $ S.map fst xs
    let phfs = S.concatMapM snd . S.filter ((hfs==) . fst) $ xs
    hs phfs
  go funL funR (x,ys) c = (funL x c, ys >>= return . S.map (\y -> funR y c))
{-# INLINE (<**) #-}

fillTable2 (Z:.(MTbl _ tbl, f)) = do
  let (_,Z:.PointL(0:.n1):.PointL(0:.n2)) = boundsM tbl
  --forM_ [1 .. n1] $ \k1 -> forM_ [1 .. n2] $ \k2 -> do
  forM_ [0 .. n1] $ \k1 -> forM_ [0 .. n2] $ \k2 -> do
    (f $ Z:.pointL 0 k1:.pointL 0 k2) >>= writeM tbl (Z:.pointL 0 k1:.pointL 0 k2)
{-# INLINE fillTable2 #-}
-}

