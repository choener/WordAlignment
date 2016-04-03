
-- | Some common functions and things that are not of immediate importance to
-- understand the algorithms.

module Linguistics.WordAlignment.Common where

import           Control.Monad (forM_)
import           Data.ByteString (ByteString)
import           Data.Char
import           Data.List (transpose,reverse)
import qualified Data.ByteString.Short as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Builder as TL
import           Data.Text (Text)
import qualified Data.Text.Format as TF
import qualified Data.Text.Encoding as T
import           Text.Printf
import           Data.Stringable (toString)
import           GHC.IO.Handle

import           NLP.Text.BTI



type IMCp = (BTI, BTI)

-- | Actually align something prettily

alignPretty :: [[IMCp]] -> [String]
alignPretty xss = map concat . transpose . map (\xs -> map (f xs) xs) . transpose . map reverse $ xss where
  f zs x = printAligned x zs

-- | Prettyprint ``characters'', which are actually small bytestrings.

printAligned = printAlignedPad ' '

-- | Print with special padding character

printAlignedPad :: Char -> IMCp -> [IMCp] -> String
printAlignedPad p (_,c) zs = printf " %s%s" (replicate pad p) (toUtf8String c) where
  pad :: Int
  pad = (1+) . maximum $ 0 : map (\(_,x) -> printLength x - printLength c) zs

-- | Length in /printed characters/ of an UTF8 string wrapped as a 'ByteString'
--
-- NOTE 'isMark' selects unicode symbols that modify a character, thereby not
-- increasing the length of the /printed/ string.

printLength :: BTI -> Int
printLength = length . filter isAN . toUtf8String where
  isAN c = not (isMark c) -- isAlphaNum c || c `elem` [ '\\', '\'', '^', '$', '-', '\'' ]


{-
  } where prnt x z = let pad = max 0 (length (filter isAN $ pp z) - length (filter isAN $ pp x))
                     in  printf " %s%s" (replicate pad ' ') (pp x)
          ds   x = ' ' : replicate (length $ filter isAN $ pp x) '-'
          isAN c = isAlphaNum c || c `elem` [ '\\', '\'' ]
-}


toUtf8String :: BTI -> String
toUtf8String = toString -- T.unpack . T.decodeUtf8 . conv
{-# INLINE toUtf8String #-}

buildLines :: [[Text]] -> TL.Builder
buildLines xss = s where
  n = (1+) . maximum $ 1 : (map (T.length . T.filter (not . isMark)) . concat $ xss)
  yss = transpose xss
  fmt = "%" ++ show n ++ "s"
  s = mconcat [ (mconcat $ map (TF.left n ' ') ys) `mappend` "\n"
              | ys <- yss ]

printLines :: Handle -> [[Text]] -> IO ()
printLines hndl xss = do
  let n = (1+) . maximum $ 1 : (map (T.length . T.filter (not . isMark)) . concat $ xss)
  let yss = transpose xss
  let fmt = "%" ++ show n ++ "s"
  let s = mconcat [ (mconcat $ map (TF.left n ' ') ys) `mappend` "\n"
                  | ys <- yss ]
  TL.hPutStrLn hndl $ TL.toLazyText s
  --forM_ yss $ \ys -> do
  --  forM_ ys $ \y -> hPrintf hndl fmt (T.unpack y)
  --  hPrintf hndl "\n"

--conv = S.fromShort . getMultiChar . uninternMultiChar
--{-# INLINE conv #-}

