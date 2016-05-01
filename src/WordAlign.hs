
{-# Options_GHC -fno-cse #-}

module Main where

import           Control.Arrow ((***),(&&&))
import           Control.Lens
import           Control.Monad (forM_,when)
import           Control.Monad.Trans.Class (lift)
import           Data.Default
import           Data.FileEmbed
import           Data.Stringable (toString)
import           Data.Version (showVersion)
import           Pipes (for)
import           Prelude hiding (Word)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           System.Console.CmdArgs hiding (def)
import           System.Exit
import           System.IO (stderr, stdout, stdin)
import           System.Mem (performGC)
import           Text.Printf

import           NLP.Scoring.SimpleUnigram
import           NLP.Scoring.SimpleUnigram.Import
import           NLP.Text.BTI

import           Linguistics.WordAlignment
import           Linguistics.WordAlignment.AlignmentBuilder (BuildAli)
import           Linguistics.WordAlignment.Bigram
import           Linguistics.WordAlignment.Common
import           Linguistics.WordAlignment.PipedPairs
import           Linguistics.WordAlignment.TwoWay.Global.Simple
import           Linguistics.WordAlignment.Word (parseWord,Word(..),addWordDelims,wordLazyTextWS,wordLazyTextWSB, fastChars, fastChar, FastChars)
import qualified Linguistics.WordAlignment.TwoWay.Global.Bigram as BI
import qualified Linguistics.WordAlignment.TwoWay.Infix.Simple as IS

import           Paths_WordAlignment (version)



data Config
  = Global2Simple
    { simpleScoreFile :: String
    , lpblock         :: Maybe (String,String)
    , showManual      :: Bool
    , filterScore     :: Maybe Double
    , filterBacktrack :: Maybe Double
    }
  | Global2Bigram
    { simpleScoreFile :: String
    , lpblock         :: Maybe (String,String)
    , showManual      :: Bool
    , filterScore     :: Maybe Double
    , filterBacktrack :: Maybe Double
    , bigramScoreFile :: String
    }
  | Infix2Simple
    { simpleScoreFile :: String
    , lpblock         :: Maybe (String,String)
    , showManual      :: Bool
    , filterScore     :: Maybe Double
    , filterBacktrack :: Maybe Double
    }
  | Infix2Bigram
    { simpleScoreFile :: String
    , bigramScoreFile :: String
    , lpblock         :: Maybe (String,String)
    , showManual      :: Bool
    , filterScore     :: Maybe Double
    , filterBacktrack :: Maybe Double
    }
  deriving (Show,Data,Typeable)

oGlobal2Simple = Global2Simple
  { simpleScoreFile = def   &= help "the file to read the simple scores from"
  , lpblock         = def   &= help "compare ONLY the given pair of languages: i.e 'Breton','Breton' or 2,3  (with the latter notation '2' being the 2nd language in the input file)"
  , showManual      = False &= help "show the manual and quit"
  , filterScore     = def   &= help "only print results with this score or higher"
  , filterBacktrack = def   &= help "only provide backtracking results for results with this score or higher"
  } &= help "Align words based on a simple, linear scoring model for gaps, and an unigram model for matches."

oGlobal2Bigram = Global2Bigram
  { simpleScoreFile = def
  , lpblock         = def
  , showManual      = False
  , filterScore     = def
  , filterBacktrack = def
  , bigramScoreFile = def   &= help "the file to read the bigram scores from"
  } &= help "Align words based on a linear scoring model for gaps, but with bigram-based scoring for matches."

oInfix2Simple = Infix2Simple
  { simpleScoreFile = def
  , lpblock         = def
  , showManual      = False
  , filterScore     = def
  , filterBacktrack = def
  } &= help "Infix-Affine grammar with simple scoring. (VERY EXPERIMENTAL, YOU HAVE BEEN WARNED)"

oInfix2Bigram = Infix2Bigram
  { simpleScoreFile = def
  , lpblock         = def
  , showManual      = False
  , filterScore     = def
  , filterBacktrack = def
  , bigramScoreFile = def
  } &= help "Infix-Affine grammar with simple scoring. (VERY EXPERIMENTAL, YOU HAVE BEEN WARNED)"

config = [oGlobal2Simple, oGlobal2Bigram, oInfix2Simple, oInfix2Bigram]
  &= program "WordAlign"
  &= summary ("WordAlign " ++ showVersion version ++ " (c) Christian Höner zu Siederdissen 2014--2016, choener@bioinf.uni-leipzig.de")
  &= verbosity

embeddedManual = $(embedFile "README.md")



main = do
  o <- cmdArgs $ modes config
  when (showManual o) $ do
    BS.putStrLn embeddedManual
    exitSuccess
  ws <- BL.getContents >>= return . V.fromList . map parseWord . BL.lines
  let !fc = fastChars 8 ws
  case o of
    Global2Simple{..} -> runGlobal2Simple o ws
    Global2Bigram{..} -> runGlobal2Bigram o ws
    Infix2Simple{..}  -> runInfix2Simple  o ws
    Infix2Bigram{..}  -> runInfix2Bigram  o ws



-- ** Global grammars.

-- | Simple global alignment.

runGlobal2Simple :: Config -> V.Vector Word -> IO ()
runGlobal2Simple = wrapSimple2IO (alignGlobalSimple2)

-- | Affine infix simple grammar

runInfix2Simple :: Config -> V.Vector Word -> IO ()
runInfix2Simple = wrapSimple2IO alignInfixSimple2

-- | Wrap simple alignments on two tapes with IO.

wrapSimple2IO
  :: BuildAli t2
  => ( SimpleScoring
       -> FastChars
       -> Int
       -> Int
       -> VU.Vector BTI
       -> VU.Vector BTI
       -> (Double, [t2])
    )
  -> Config
  -> V.Vector Word
  -> IO ()
wrapSimple2IO f cfg ws = do
  !v <- getVerbosity
  !scoring <- simpleScoreFromFile $ simpleScoreFile cfg
  let align = alignmentWrapper2 (const $ f scoring)
      -- 4 arguments, @const@ takes care of the @()@ group action result
      {-# Inline align #-}
  runAlignment
    (for  (runTwowayAlignments groupActionGC align eachGroupStatus ws)
          (lift . lift . (TL.hPutStr stdout . TL.toLazyText))
    )
    ( set aliFilterScore (filterScore cfg) .
      set aliFilterBackt (filterBacktrack cfg) .
      set aliVerbose (v==Loud) $
      def
    )



-- ** Affine grammars

-- | Global bigram grammar.

runGlobal2Bigram :: Config -> V.Vector Word -> IO ()
runGlobal2Bigram = wrapBigram2IO alignGlobalBigram2

-- | Affine infix bigram grammar.

runInfix2Bigram :: Config -> V.Vector Word -> IO ()
runInfix2Bigram = wrapBigram2IO alignInfixBigram2

-- | Wrap two-tape bigram alignments in IO.

wrapBigram2IO f cfg ws = do
  !v <- getVerbosity
  !simpleScoring <- simpleScoreFromFile $ simpleScoreFile cfg
  let chkLs = S.fromList . map wordLang . V.toList $ ws
  !bigramScoring <- BL.readFile (bigramScoreFile cfg) >>= return . mkBigramMap chkLs (-999999)
  let perGroup _ _ = do
      groupActionGC () ()
      xy <- use aliGroupLanguages
      let [x,y] = case xy of
                    [z] -> [z,z]
                    [x,y] -> [x,y]
      return $ getScores2 True bigramScoring x y
  let align = alignmentWrapper2 (f simpleScoring)
      -- 5 arguments, receives the group action result (the bigram scores)
      {-# Inline align #-}
  runAlignment
    (for  (runTwowayAlignments perGroup align eachGroupStatus ws)
          (lift . lift . (TL.hPutStr stdout . TL.toLazyText))
    )
    ( set aliFilterScore (filterScore cfg) .
      set aliFilterBackt (filterBacktrack cfg) .
      set aliVerbose (v==Loud) .
      set aliCustom (mempty :: Scores) $
      (def :: AlignmentConfig ())
    )

