
module Linguistics.WordAlignment.AlignmentBuilder where

import           Control.Lens
import           Data.List (intersperse)
import           Data.Monoid
import           Data.Text.Lazy.Builder (Builder)
import           Prelude hiding (Word)
import qualified Data.Text.Format as TF
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Vector.Unboxed as VU

import           Linguistics.WordAlignment.Common
import           Linguistics.WordAlignment.Word (Word(..), wordLazyTextWSB)



-- | Build an actual k-way alignment.

buildAlignmentBuilder :: BuildAli t => Double -> ([Word],(Double,[t])) -> Builder
buildAlignmentBuilder k (ws,(s,xss)) = {-# SCC "buildAliBuilder" #-} hdr <> "\n" <> ls <> "\n"
  where hdr = TF.build "IDS:{} SCORE: {} NSCORE: {}  {}"
                       ( wids
                       , TF.left 6 ' ' $ TF.fixed 2 s
                       , TF.left 6 ' ' $ TF.fixed 2 normScore
                       , words
                       )
        wids  = mconcat . map (TF.build " {}" . TF.Only . wordID) $ ws
        words = mconcat . map (TF.build " WORD: {}" . TF.Only . wordLazyTextWSB) $ ws
        normScore = s / (maximum $ 1 : map ((+k) . fromIntegral . VU.length . wordWord) ws)
        ls = mconcat $ map buildAli xss

-- | During backtracking, a list of tuples is created. Here, out of a list
-- of tuples, a tuple of lists is created and turned into lines of
-- builders.

class BuildAli t where
  buildAli ::t -> Builder

-- | Instance for three lines, here used for 2-way alignments with a third
-- line for scores.

instance BuildAli [(Builder, Builder, Builder)] where
  buildAli t =
    let l1 = t ^. traverse . _1
        l2 = t ^. traverse . _2
        l3 = t ^. traverse . _3
    in  l1 <> "\n" <> l2 <> "\n" <> l3 <> "\n"

-- | Four lines: 3-way alignment + score.

instance BuildAli [(Builder, Builder, Builder, Builder)] where
  buildAli t =
    let l1 = t ^. traverse . _1
        l2 = t ^. traverse . _2
        l3 = t ^. traverse . _3
        l4 = t ^. traverse . _4
    in  l1 <> "\n" <> l2 <> "\n" <> l3 <> "\n" <> l4 <> "\n"

-- | Five lines: 4-way alignment + score.

instance BuildAli [(Builder, Builder, Builder, Builder, Builder)] where
  buildAli t =
    let l1 = t ^. traverse . _1
        l2 = t ^. traverse . _2
        l3 = t ^. traverse . _3
        l4 = t ^. traverse . _4
        l5 = t ^. traverse . _5
    in  l1 <> "\n" <> l2 <> "\n" <> l3 <> "\n" <> l4 <> "\n" <> l5 <> "\n"

