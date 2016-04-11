
-- | Functions of alignments of words.

module Linguistics.WordAlignment
  ( module Linguistics.WordAlignment
  , buildAlignmentBuilder
  ) where

import           Data.Text.Lazy.Builder (Builder)
import           Data.Vector (Vector)
import           Pipes hiding ((<~))
import           Prelude hiding (Word)
import           Data.Default
import           Control.Monad.State.Strict
import qualified Data.Vector as V
import           Data.List (groupBy)
import           Data.Function (on)
import           Control.Arrow (second)
import           Control.Lens hiding (each)
import           Control.DeepSeq

import           Data.Vector.Combined

import qualified Linguistics.WordAlignment.TwoWay.Infix.Simple as IS2
import qualified Linguistics.WordAlignment.TwoWay.Infix.Bigram as IB2
import           Linguistics.WordAlignment.AlignmentBuilder
import           Linguistics.WordAlignment.Word (Word, wordLang, wordWord, FastChars, fastChars)
import           Linguistics.WordAlignment.PipedPairs



alignInfixSimple2 = IS2.alignInfix
alignInfixBigram2 = IB2.alignInfix

