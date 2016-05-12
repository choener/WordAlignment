
-- | Functions of alignments of words.

module Linguistics.WordAlignment
  ( module Linguistics.WordAlignment
  , buildAlignmentBuilder
  ) where

import           Control.Arrow (second)
import           Control.DeepSeq
import           Control.Lens hiding (each)
import           Control.Monad.State.Strict
import           Data.Default
import           Data.Function (on)
import           Data.List (groupBy)
import           Data.Text.Lazy.Builder (Builder)
import           Data.Vector (Vector)
import           Pipes hiding ((<~))
import           Prelude hiding (Word)
import qualified Data.Vector as V
import           System.IO (stderr)
import           Text.Printf

import           Data.Vector.Combined
import           NLP.Text.BTI

import           Linguistics.WordAlignment.AlignmentBuilder
import           Linguistics.WordAlignment.PipedPairs
import           Linguistics.WordAlignment.Word (Word, wordLang, wordWord, FastChars, fastChars)
import qualified Linguistics.WordAlignment.TwoWay.Global.Bigram as GB2
import qualified Linguistics.WordAlignment.TwoWay.Global.Simple as GS2
import qualified Linguistics.WordAlignment.TwoWay.Infix.Bigram as IB2
import qualified Linguistics.WordAlignment.TwoWay.Infix.Simple as IS2



alignGlobalSimple2 = GS2.alignGlobal
alignGlobalBigram2 = GB2.alignGlobal

alignInfixSimple2 = IS2.alignInfix
alignInfixBigram2 = IB2.alignInfix

-- | Wrap up the most common way to perform alignments. The function @f@
-- needs to have all scoring parts incorporated.

alignmentWrapper2 f fga fc x y = do
  w <- use aliWidth
  c <- use aliNumCoopts
  sf <- use aliFilterScore
  bf <- use aliFilterBackt
  let (d,bts) = f fga fc w c (wordWord x) (wordWord y)
  let ali = scoreFilter sf d
          $ buildAlignmentBuilder (-1) ([x,y],(d, btFilter False bf d bts))
  return ali
{-# Inline alignmentWrapper2 #-}

-- |

scoreFilter (Just z) d blder | z > d = mempty
scoreFilter _        _ blder = blder
{-# Inline scoreFilter #-}

-- |

btFilter True _        d xs = []
btFilter _    (Just z) d xs | z > d = []
btFilter _    _        _ xs = xs
{-# Inline btFilter #-}

-- | Default system for printing out the status every 10k alignments.

eachGroupStatus len k x y = do
  let wLx :: String = btiToCS $ wordLang x
      wLy :: String = btiToCS $ wordLang y
  v <- use aliVerbose
  numGs <- use aliGroups
  curG  <- use aliCurGroup
  lift . when (v && k `mod` 10000 == 9999)
       $ hPrintf stderr "%5d %5d   %s %s %10d %10d\n" numGs curG wLx wLy len (k+1)
{-# Inline eachGroupStatus #-}

-- | In case we don't want to do anything for each group.

eachGroupNothing _ _ _ _ = return ()

