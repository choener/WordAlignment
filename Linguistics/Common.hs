
-- | Some common functions and things that are not of immediate importance to
-- understand the algorithms.

module Linguistics.Common where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Text.Printf
import qualified Data.Text.Encoding as T
import Data.Char
import Data.List (transpose)



-- | Actually align something prettily

alignPretty :: [[ByteString]] -> [String]
alignPretty xss = map concat . transpose . map (\xs -> map (f xs) xs) . transpose $ xss where
  f zs x = printAligned x zs

-- | Prettyprint ``characters'', which are actually small bytestrings.

printAligned = printAlignedPad ' '

-- | Print with special padding character

printAlignedPad :: Char -> ByteString -> [ByteString] -> String
printAlignedPad p c zs = printf " %s%s" (replicate pad p) (toUtf8String c) where
  pad :: Int
  pad = (1+) . maximum $ 0 : map (\x -> printLength x - printLength c) zs

-- | Length in /printed characters/ of an UTF8 string wrapped as a 'ByteString'
--
-- NOTE 'isMark' selects unicode symbols that modify a character, thereby not
-- increasing the length of the /printed/ string.

printLength :: ByteString -> Int
printLength = length . filter isAN . toUtf8String where
  isAN c = not (isMark c) -- isAlphaNum c || c `elem` [ '\\', '\'', '^', '$', '-', '\'' ]


{-
  } where prnt x z = let pad = max 0 (length (filter isAN $ pp z) - length (filter isAN $ pp x))
                     in  printf " %s%s" (replicate pad ' ') (pp x)
          ds   x = ' ' : replicate (length $ filter isAN $ pp x) '-'
          isAN c = isAlphaNum c || c `elem` [ '\\', '\'' ]
-}


toUtf8String :: ByteString -> String
toUtf8String = T.unpack . T.decodeUtf8
{-# INLINE toUtf8String #-}

