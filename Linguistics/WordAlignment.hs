
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
import           Linguistics.WordAlignment.Word (Word, wordLang, FastChars, fastChars)



alignInfixSimple2 = IS2.alignInfix
alignInfixBigram2 = IB2.alignInfix

-- |
--
-- TODO runAlignment should return one big builder.
--
-- TODO can we generalize a bit more so that we can get the alignments
-- themselves? or should we assume that people will call alignInfixSimple2
-- themselves?

runInfix2Simple
  :: _groupAction
  -> Int
  -> _scoring
  -> _verbose
  -> Builder
runInfix2Simple groupAction width scoring v
  !scoring <- simpleScoreFromFile simpleScoreFile
  !v <- getVerbosity
  let groupAction _ _ = lift performGC
      {-# Inline groupAction #-}
  let alignXY () fc x y =
        let (d,bts) = alignInfixSimple2 fc 8 scoring (wordWord x) (wordWord y) 1
            ali = scoreFilter filterScore d
                $ buildAlignmentBuilder (-1) ([x,y],(d, btFilter False filterBacktrack d bts))
        in  return ali
      {-# Inline alignXY #-}
  let eachXY len k x y =
        let wLx = toString $ wordLang x
            wLy = toString $ wordLang y
        in  do numGs <- use aliGroups
               curG  <- use aliCurGroup
               lift . when (v==Loud && k `mod` 10000 == 9999)
                    $ hPrintf stderr "%5d %5d   %s %s %10d %10d\n" numGs curG wLx wLy len (k+1)
      {-# Inline eachXY #-}
  runAlignment
    (for  (runTwowayAlignments groupAction alignXY eachXY ws)
          (lift . lift . (TL.hPutStr stdout . TL.toLazyText))
    )
    (AlignmentConfig width 0 0 ())


