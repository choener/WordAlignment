
module Linguistics.WordAlignment.FastLookups where

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy as TL
import           Data.Text.Encoding (encodeUtf8)
import           Control.DeepSeq
import qualified Data.ByteString as B



data FastDoubles = FastDoubles
  { fcTable :: !(HM.HashMap Double B.ByteString)
  , fcWidth :: !Int
  }

-- | Generate the fast lookup table

fastDoubles :: Int -> [Double] -> FastDoubles
fastDoubles width ds = {-# SCC "fastDoubles" #-} deepseq ds `seq` FastDoubles hm width
  where hm = HM.fromList . map fmt $ ds
        fmt k = (k , encodeUtf8 . TL.toStrict . TF.format "{}" . TF.Only . TF.left width ' ' $ TF.fixed 1 k)
{-# NoInline fastDoubles #-}

fastDouble :: FastDoubles -> Double -> BB.Builder -- TLB.Builder
fastDouble (FastDoubles hm width) k = {-# SCC "fastChar" #-} maybe encodek BB.byteString $ HM.lookup k hm
  where encodek = BB.byteString . encodeUtf8 . TL.toStrict . TF.format "{}" . TF.Only . TF.left width ' ' $ TF.fixed 1 k
{-# InlineAble fastDouble #-}

