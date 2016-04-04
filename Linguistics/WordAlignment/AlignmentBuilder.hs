
module Linguistics.WordAlignment.AlignmentBuilder where

import           Control.Lens
import           Data.Monoid
import           Prelude hiding (Word)
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Vector.Unboxed as VU

import           Linguistics.WordAlignment.Common
import           Linguistics.WordAlignment.Word (Word(..), wordLazyTextWSB)

-- |

buildAlignmentBuilder :: Double -> ([Word],(Double,[[B3]])) -> TL.Builder
buildAlignmentBuilder k (ws,(s,xss)) = {-# SCC "buildAliBuilder" #-} hdr <> wds <> "\n" <> ls <> "\n"
  where hdr = {-# SCC "buildAliBuilder/hdr" #-}
              TF.build "IDS: {} {} SCORE: {} NSCORE: {}    WORD: "
                       (wid0, wid1, TF.left 6 ' ' $ TF.fixed 2 s, TF.left 6 ' ' $ TF.fixed 2 normScore)
        wds = wordLazyTextWSB (ws!!0) <> "   WORD: " <> wordLazyTextWSB (ws!!1)
        normScore = s / (maximum $ 1 : map ((+k) . fromIntegral . VU.length . wordWord) ws)
        wid0 = wordID $ ws!!0
        wid1 = wordID $ ws!!1
        ls = mconcat $ map buildAli xss
        buildAli b3s = let l1 = b3s ^. traverse . _1
                           l2 = b3s ^. traverse . _2
                           l3 = b3s ^. traverse . _3
                       in  l1 <> "\n" <> l2 <> "\n" <> l3 <> "\n"

