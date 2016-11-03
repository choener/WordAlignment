
module Main where

import           Control.Monad (forM,unless)
import           Data.List (intersperse)
import           Data.List.Split (splitOneOf)
import           Debug.Trace
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy.IO as TL
import           System.FilePath ((</>),(<.>))
import           System.IO (stdout)
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.Silver as S
import           Test.Tasty.Silver.Interactive as SI
import           Test.Tasty.TH
import           Data.List (isInfixOf)

import           Linguistics.WordAlignment.Bigram
import           Linguistics.WordAlignment.Word (parseWord,Word(..),addWordDelims,wordLazyTextWS,wordLazyTextWSB, FastChars(..))
import           NLP.Scoring.SimpleUnigram
import           NLP.Scoring.SimpleUnigram.Import

import           Linguistics.WordAlignment
import           Linguistics.WordAlignment.FastLookups



{-
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
-}

-- Test files are split according to this scheme:
--
-- @
-- directory name "/"
-- grammartype
-- unigram or bigram
-- unigram or bigram score name
-- words file
-- ".golden"
-- @

runSingleTest gldn [dir,grammar,"unigram",ugms,wrds,cnt,suffix] = do
  let c = read cnt
  unless (grammar `elem` ["global","infix"]) $ error gldn
  -- words file
  ws <- (map parseWord . BL.lines) <$> BL.readFile (dir </> wrds <.> "words")
  -- the bigram-associated simple scoring file
  simpleScoring <- simpleScoreFromFile $ dir </> ugms <.> "ugdef"
  -- a particular way we do scores for all inputs
  let xys = [ (x,y) | x <- ws, y <- ws, x <= y ]
  ts <- forM xys $ \ (x,y) -> do
    let fc = FastChars mempty 8
    let fd = FastDoubles mempty 8
    let (d,bts) = case grammar of
          "global" -> alignGlobalSimple2 simpleScoring fc fd 8 c (wordWord x) (wordWord y)
          "infix"  -> alignInfixSimple2 simpleScoring fc fd 8 c (wordWord x) (wordWord y)
    let ali = buildAlignmentBuilder 0 ([x,y],(d, bts))
    let hndl = stdout
    return $ BB.toLazyByteString ali
  let res = TL.toStrict . TLE.decodeUtf8 . mconcat $ ts
  return res


runSingleTest gldn [dir,grammar,"bigram",bgms,wrds,cnt,suffix] = do
  let c = read cnt
  unless (grammar `elem` ["global","infix"]) $ error gldn
  -- words file
  ws <- (map parseWord . BL.lines) <$> BL.readFile (dir </> wrds <.> "words")
  -- the bigram scoring file
  let chkLs = S.fromList . map wordLang $ ws
  bigramScoring <- BL.readFile (dir </> bgms <.> "bgms") >>= return . mkBigramMap chkLs (-999999)
  -- the bigram-associated simple scoring file
  simpleScoring <- simpleScoreFromFile $ dir </> bgms <.> "bgdef"
  -- a particular way we do scores for all inputs
  let xys = [ (x,y) | x <- ws, y <- ws, x <= y ]
  ts <- forM xys $ \ (x,y) -> do
    let fc = FastChars mempty 8
    let fd = FastDoubles mempty 8
    let !sco = getScores2 False bigramScoring (wordLang x) (wordLang y)
    let (d,bts) = case grammar of
          "global" -> alignGlobalBigram2 simpleScoring sco fc fd 8 c (wordWord x) (wordWord y)
          "infix"  -> alignInfixBigram2 simpleScoring sco fc fd 8 c (wordWord x) (wordWord y)
    let ali = buildAlignmentBuilder 0 ([x,y],(d, bts))
    let hndl = stdout
    return $ BB.toLazyByteString ali
  let res = TL.toStrict . TLE.decodeUtf8 . mconcat $ ts
  return res

runSingleTest gldn xs = do
  error $ "don't know how to execute test based on: " ++ gldn

testWrapper gldn = S.goldenVsAction name gldn (runSingleTest gldn xs) id
  where name = concat . intersperse "-" . drop 1 . take (length xs - 2) $ xs
        xs   = splitOneOf "/-." gldn

main :: IO ()
main = do
  gg <-  testGroup "Known good alignments"
     <$> (fmap testWrapper) -- . filter ("order" `isInfixOf`))
     <$> S.findByExtension [".golden"] "tests"
  SI.defaultMain gg

