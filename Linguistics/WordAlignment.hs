
-- | Functions of alignments of words.

module Linguistics.WordAlignment
  ( alignInfixSimple2
  , alignInfixBigram2
  ) where

import qualified Linguistics.WordAlignment.TwoWay.Infix.Simple as IS2
import qualified Linguistics.WordAlignment.TwoWay.Infix.Bigram as IB2

alignInfixSimple2 = IS2.alignInfix
alignInfixBigram2 = IB2.alignInfix

