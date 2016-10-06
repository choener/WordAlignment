
module Main where

import           Control.Monad (forM)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TL
import           System.IO (stdout)
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.Silver as S
import           Test.Tasty.Silver.Interactive as SI
import           Test.Tasty.TH

import           Linguistics.WordAlignment.Bigram
import           Linguistics.WordAlignment.Word (parseWord,Word(..),addWordDelims,wordLazyTextWS,wordLazyTextWSB, FastChars(..))
import           NLP.Scoring.SimpleUnigram
import           NLP.Scoring.SimpleUnigram.Import

import           Linguistics.WordAlignment
import           Linguistics.WordAlignment.FastLookups



infixBigramTest = do
  ws <- (map parseWord . BL.lines) <$> BL.readFile "tests/example.words"
  simpleScoring <- simpleScoreFromFile "scores/defaultBigramScoring"
  let chkLs = S.fromList . map wordLang $ ws
  bigramScoring <- BL.readFile "tests/example.bgms" >>= return . mkBigramMap chkLs (-999999)
  ts <- forM ws $ \x -> forM ws $ \y -> do
    let fc = FastChars mempty 8
    let fd = FastDoubles mempty 8
    let !sco = getScores2 False bigramScoring (wordLang x) (wordLang y)
    let (d,bts) = alignInfixBigram2 simpleScoring sco fc fd 8 1 (wordWord x) (wordWord y)
    let ali = buildAlignmentBuilder 0 ([x,y],(d, bts))
    let hndl = stdout
    return $ BB.toLazyByteString ali
  return . TL.toStrict . TLE.decodeUtf8 . mconcat $ concat ts

goldenInfixBigramTest
  = S.goldenVsAction
      "Infix-Bigram"
      "tests/infix-bigram.golden"
      infixBigramTest
      id


main :: IO ()
main = do
  SI.defaultMain goldenInfixBigramTest
  $(defaultMainGenerator)

