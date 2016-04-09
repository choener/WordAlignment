
{-# Options_GHC -fno-cse #-}

module Main where

import           Control.Arrow ((***),(&&&))
import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_,when)
import           Control.Parallel.Strategies (rdeepseq,parMap,parBuffer,using,evalTuple2,r0,rseq,evalBuffer,parList,evalList,evalTuple3,evalTuple5)
import           Data.Aeson (encode)
import           Data.FileEmbed
import           Data.Function (on)
import           Data.List (sortBy,groupBy,intersperse,genericLength)
import           Data.Maybe (isJust)
import           Data.Sequence (Seq)
import           Data.Strict.Tuple
import           Data.Text (Text)
import           Data.Version (showVersion)
import           Debug.Trace
import           Debug.Trace (trace)
import           GHC.Conc (numCapabilities)
import           GHC.IO.Handle
import           Prelude hiding (Word)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           System.Console.CmdArgs
import           System.Exit
import           System.IO
import           System.Mem (performGC)
import           Text.Printf
import           Text.Read (readMaybe)
import           Data.Stringable (toString)
import qualified Data.Text.Format as TF
import           Data.Monoid ((<>))
import           Control.Lens

import           NLP.Scoring.SimpleUnigram
import           NLP.Scoring.SimpleUnigram.Import
import           NLP.Text.BTI

import           Linguistics.WordAlignment
import           Linguistics.WordAlignment.Bigram
import           Linguistics.WordAlignment.Common
import           Linguistics.WordAlignment.TwoWay.Global.Simple
import           Linguistics.WordAlignment.Word (parseWord,Word(..),addWordDelims,wordLazyTextWS,wordLazyTextWSB, fastChars, fastChar, FastChars)
import qualified Linguistics.WordAlignment.TwoWay.Global.Bigram as BI
import qualified Linguistics.WordAlignment.TwoWay.Infix.Simple as IS

import           Paths_WordAlignment (version)



data Config
  = Global2Simple
    { scoreFile     :: String
    , lpblock       :: Maybe (String,String)
    , showManual    :: Bool
    , prettystupid  :: Bool
    , outfile       :: String
    , nobacktrack   :: Bool
    }
  | Global2Bigram
    { scoreFile     :: String
    , bigramDef     :: Double
    , gapOpen       :: Double
    , lpblock       :: Maybe (String,String)
    , showManual    :: Bool
    , prettystupid  :: Bool
    , outfile       :: String
    , nobacktrack   :: Bool
    , serialized    :: Bool
    , filterScore   :: Maybe Double
    , filterBacktrack :: Maybe Double
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
  { scoreFile  = def &= help "the file to read the simple scores from"
  , lpblock    = Nothing
  , showManual = False    &= help "show the manual and quit"
  , prettystupid = False
  , outfile      = ""
  , nobacktrack   = False
  } &= help "Align words based on a simple, linear scoring model for gaps, and an unigram model for matches."

oGlobal2Bigram = Global2Bigram
  { scoreFile  = "" &= help "the file to read the scores from"
  , bigramDef  = (-20) &= help "score to use for unknown bigram matches"
  , gapOpen    = (-5) &= help "cost to open a gap"
  , lpblock    = Nothing  &= help "compare ONLY the given pair of languages: i.e 'Breton','Breton' or 2,3  (with the latter notation '2' being the 2nd language in the input file)"
  , prettystupid = False  &= help "a pretty stupid developer option"
  , outfile      = ""     &= help "write output to this file"
  , showManual = False
  , nobacktrack   = False
  , serialized = False
  , filterScore = Nothing &= help "only print results with this score or higher"
  , filterBacktrack = Nothing &= help "only provide backtracking results for results with this score or higher"
  } &= help "Align words based on a linear scoring model for gaps, but with bigram-based scoring for matches."

oInfix2Simple = Infix2Simple
  { simpleScoreFile = def
  , lpblock         = def
  , showManual      = def
  , filterScore     = def
  , filterBacktrack = def
  } &= help "Infix-Affine grammar with simple scoring. (VERY EXPERIMENTAL, YOU HAVE BEEN WARNED)"

oInfix2Bigram = Infix2Bigram
  { simpleScoreFile = def
  , bigramScoreFile = def
  , lpblock         = def
  , showManual      = def
  , filterScore     = def
  , filterBacktrack = def
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
  hSetBuffering stdin  $ LineBuffering
  hSetBuffering stdout $ LineBuffering
  hSetBuffering stderr $ LineBuffering
  ws <- BL.getContents >>= return . V.fromList . map parseWord . BL.lines
  let !fc = fastChars 8 ws
  case o of
    Global2Simple{..} -> run2Simple o (blockSelection2 lpblock ws)
    Global2Bigram{..} -> run2 o (blockSelection2 lpblock $ V.map addWordDelims ws)
    Infix2Simple{..}  -> runInfix2Simple o fc $ blockSelection2 lpblock ws
    Infix2Bigram{..}  -> runInfix2Bigram o fc $ blockSelection2 lpblock $ V.map addWordDelims ws



-- | Affine infix simple grammar

runInfix2Simple :: Config -> FastChars -> WSS -> IO ()
runInfix2Simple o@Infix2Simple{..} fc wss = do
  hndl <- return stdout
  scoring <- simpleScoreFromFile simpleScoreFile
  v <- getVerbosity
  let wsslen = length wss
  -- for each language pair
  forM_ (zip [1::Int ..] wss) $ \(langNumber,(len,ws)) -> do
    let (wLx,wLy) = (toString . wordLang *** toString . wordLang) $ head ws
    performGC
    forM_ (zip [1::Int ..] ws) $ \(k,(x,y)) -> {-# SCC "runInfix2S/forM_/ws" #-} do
      let (d,bts) = alignInfixSimple2 fc 8 scoring (wordWord x) (wordWord y) 1
      let ali = scoreFilter filterScore d $ buildAlignmentBuilder (-1) ([x,y],(d, btFilter False filterBacktrack d bts))
      when (v==Loud && k `mod` 10000 == 0) $ hPrintf stderr "%s %s %10d %10d\n" wLx wLy len k
      TL.hPutStr hndl $ TL.toLazyText ali

-- | Affine infix bigram grammar

runInfix2Bigram :: Config -> FastChars -> WSS -> IO ()
runInfix2Bigram o@Infix2Bigram{..} fc wss = do
  hndl <- return stdout
  simpleScoring <- simpleScoreFromFile simpleScoreFile
  let chkLs = S.fromList . map wordLang . concat . map (\(x,y) -> [x,y]) . map (head . Prelude.snd) $ wss
  bigramScoring <- BL.readFile bigramScoreFile >>= return . generateLookups chkLs (-999999)
  v <- getVerbosity
  let wsslen = length wss
  -- for each language pair
  forM_ (zip [1::Int ..] wss) $ \(langNumber,(len,ws)) -> do
    let (wLx,wLy) = (toString . wordLang *** toString . wordLang) $ head ws
    let (hx,hy) = head ws
    let !sco = getScores2 True bigramScoring (wordLang hx) (wordLang hy)
    performGC
    forM_ (zip [1::Int ..] ws) $ \(k,(x,y)) -> {-# SCC "runInfix2S/forM_/ws" #-} do
      let (d,bts) = alignInfixBigram2 fc 8 simpleScoring sco (wordWord x) (wordWord y) 1
      let ali = scoreFilter filterScore d $ buildAlignmentBuilder (-1) ([x,y],(d, btFilter False filterBacktrack d bts))
      when (v==Loud && k `mod` 10000 == 0) $ hPrintf stderr "%s %s %10d %10d\n" wLx wLy len k
      TL.hPutStr hndl $ TL.toLazyText ali


-- | Given a @Config@ and a @List of Lists of Word-Pairs@ align everything.

run2Simple :: Config -> WSS -> IO ()
run2Simple Global2Simple{..} wss = do
  error "write along the lines of infix2s / infix2b"
  {-
  hndl <- if null outfile then return stdout else openFile outfile AppendMode
  scoring <- simpleScoreFromFile scoreFile
  let wsslen = length wss
  -- for each language pairing
  forM_ (zip [1::Int ..] wss) $ \(k,(len,ws)) -> do
    let (wLx,wLy) = (toString . wordLang *** toString . wordLang) $ head ws
    performGC
    {-
    pg <- if prettystupid
            then do
              let (x,y) = head ws
              printf "[%4d / %4d] Language pair: %s / %s with %d alignments:\n" k wsslen (show $ wordLang x) (show $ wordLang y) len
              Just <$> CAP.newProgressBar CAP.def { CAP.pgWidth = 100, CAP.pgTotal = len }
            else return Nothing
    -}
    let alis = [ (x,y,d,bts,sss)
               | (x,y) <- ws
               , let (d,bts') = alignGlobal scoring 1 (wordWord x) (wordWord y)  -- calculate score and all co-optimal backtraces
               , let bts = if nobacktrack then [] else bts'
               , let sss = TL.toLazyText $ buildAlignmentSimple 0 ([x,y],(d,bts)) -- make nice strings
               ]
    -- print the actual alignments
    forM_ alis $ \(x,y,d,bts,sss) -> do
      when (prettystupid && k `mod` 10000 == 0) $ printf "%s %s %10d %10d\n" wLx wLy len k
      TL.hPutStr hndl sss
      --when (isJust pg) $ let Just pg' = pg in CAP.tick pg'
-}

-- | Given a @Config@ and a @List of List of Word-Pairs@ align everything.
--
-- TODO Can we get around explicitly forcing the outer spine and the first
-- element of each inner pairing?

run2 :: Config -> WSS -> IO ()
run2 Global2Bigram{..} wss = {-# SCC "run2" #-} do
  error "write along the lines of infix2s / infix2b"
{-
  hndl <- if null outfile then return stdout else openFile outfile AppendMode
  let wsslen = length wss
  -- build up scoring system. This will force the spine of the
  -- language-pairing list but should not force the pairs explicitly.
  let chkLs = S.fromList . map wordLang . concat . map (\(x,y) -> [x,y]) . map (head . Prelude.snd) $ wss
  scoring <- BL.readFile scoreFile >>= return . generateLookups chkLs (-999999)
  -- for each language pairing
  forM_ (zip [1::Int ..] wss) $ \(tcnt,(len,ws)) -> do
    let (wLx,wLy) = (toString . wordLang *** toString . wordLang) $ head ws
    performGC
    -- get score pairing
    let (hx,hy) = head ws
    let !sco = {-# SCC "run2/sco" #-} getScores2 True scoring (wordLang hx) (wordLang hy)
    if not serialized
    then do
      -- align the words the in @ws@ pairing
      let as = {-# SCC "run2/as" #-}
                map (\(x,y) -> ( let (d,bts) = BI.alignGlobal 8 bigramDef gapOpen sco 1 (wordWord x) (wordWord y)
                                 in  seq d $ scoreFilter filterScore d $ buildAlignmentBuilder (-1) ([x,y],(d, btFilter nobacktrack filterBacktrack d bts))
                               )
                    ) ws
      forM_ (zip [1::Int ..] as) $ \(k,ali) -> {-# SCC "run2/IO" #-} do
        when (prettystupid && k `mod` 10000 == 0) $ printf "%s %s || %7d %7d || %10d %10d\n" wLx wLy wsslen tcnt len k
        TL.hPutStr hndl $ TL.toLazyText ali
    else do
      let as = {-# SCC "run2/ser" #-}
                map (\(x,y) -> ( let (d,_) = BI.alignGlobal 8 bigramDef gapOpen sco 0 (wordWord x) (wordWord y)
                                 in  (wordID x, wordID y, d)
                               )
                    ) ws
      BL.putStrLn $ encodeAlignedSet ws as
-}

scoreFilter (Just z) d blder | z > d = mempty
scoreFilter _        _ blder = blder

btFilter True _        d xs = []
btFilter _    (Just z) d xs | z > d = []
btFilter _    _        _ xs = xs

-- | Given a set of words from different languages, we want to do two
-- things:
--
-- (i) We want to block alignments into groups. This will make alignments
-- faster as we do not have to reload the scoring system every time.
--
-- (ii) We want to decide on alignment maybe only a subset of words. We
-- make this selection rather coarse-grained by giving just the name or
-- running id of the language. I.e you can type @Breton@ or @1@ (if Breton
-- happens to be the first language). We allow numeric identification as
-- that is easier for scripts to handle.
--
-- (iii) We calculate the length of each language-pairing explicitly, not
-- from @length list@ so to not force the list spine too early.

blockSelection2 :: Maybe (String,String) -> V.Vector Word -> WSS
blockSelection2 s ws = {-# SCC "blockSelection2" #-} filter (not . null . Prelude.snd) $ go (mkCmp s)
        -- grouping words by their languages, pair each language group with
        -- an index
  where gs = zip [1..] $ groupBy ((==) `on` wordLang) $ V.toList ws
        -- length of each group
        lgs = VU.fromList $ (-1) : map (length . Prelude.snd) gs
        -- Gives a map "Language String Name" -> Int (for the gs)
        ls = M.fromList $ map (\(k,(v:_)) -> (show $ wordLang v,k)) gs
        -- what to store as length
        calcLength k l
          | k /= l    = lgs VU.! k * lgs VU.! l
          | otherwise = (lgs VU.! k -1) * lgs VU.! l `div` 2
        -- produces the word pairs to be aligned
        go f = [ (calcLength k l,[ (x,y)
                 | (kk,x) <- zip [1..] xs, (ll,y) <- zip [1..] ys   -- actually enumerate the words
                 , k/=l || kk < ll                                  -- upper-tri for same group, otherwise all alignments
                 ])
               | (k,xs) <- gs, (l,ys) <- gs -- @k@ and @l@ are word groups, i.e. language identifiers
               , f k l                      -- shall we accept this language combination
               ]
        -- Create comparator function for group selection
        mkCmp :: Maybe (String,String) -> (Int -> Int -> Bool)
        -- nothing selected, produce all upper triangular groups
        mkCmp Nothing = \k l -> k <= l
        mkCmp (Just (a,b))
        -- we have two strings that actually are numbers
          | Just a' <- readMaybe a
          , Just b' <- readMaybe b = \k l -> a'==k && b'==l
        -- we have two language names
          | Just a' <- M.lookup a ls
          , Just b' <- M.lookup b ls = \k l -> a'==k && b'==l
        -- the user did provide crappy input
        mkCmp (Just (a,b)) = \k l -> traceShow ("Unknown languages or ID's: " ++ a ++ " , " ++ b) $ False

type WSS = [(Int,[(Word,Word)])] -- V.Vector (V.Vector (Word,Word))

-- | (write me)

prettyAli2 :: Double -> [(BTI,BTI)] -> IO ()
prettyAli2 d s = do
  print d
  forM_ s $ \(x,xs) -> do
    putStr $ show x
    putStr $ show xs
  putStrLn ""
  forM_ s $ \(_,y) -> do
    putStr $ show y
  putStrLn ""

buildAlignmentSimple :: Double -> ([Word],(Double,[[[Text]]])) -> TL.Builder
buildAlignmentSimple k (ws,(s,(xs))) = TL.fromText hdr `mappend` ls `mappend` "\n" where
  ids = concat . intersperse " " . map (show . wordID)   $ ws
  wds = concat . intersperse "   WORD   " . map (concat . intersperse " " . map toUtf8String . VU.toList . wordWord) $ ws
  ns = s / (maximum $ 1 : map ((+k) . fromIntegral . VU.length . wordWord) ws)
  hdr = T.pack $ printf "IDS: %s SCORE: %.2f NSCORE: %.2f    WORDS: %s\n" ids s ns wds
  ls  = case xs of [] -> "" ; [xs'] -> buildLines xs'

buildAlignment :: Double -> ([Word],(Double,[[[Text]]])) -> TL.Text
buildAlignment k (ws,(s,(xss)))
  = {-# SCC "pretty_ali" #-} TL.toLazyText $ TL.fromLazyText hdr <> wds <> "\n" <> ls <> "\n"
    where
      wds = wordLazyTextWSB (ws!!0) <> "   WORD: " <> wordLazyTextWSB (ws!!1)
      ns = s / (maximum $ 1 : map ((+k) . fromIntegral . VU.length . wordWord) ws)
      hdr = TF.format "IDS: {} {} SCORE: {} NSCORE: {}    WORD: "
                (wid0, wid1, TF.left 6 ' ' $ TF.fixed 2 s, TF.left 6 ' ' $ TF.fixed 2 ns)
      wid0 = wordID $ ws!!0
      wid1 = wordID $ ws!!1
      ls  = case xss of [] -> "" ; [xs'] -> buildLines $ ["^","^","0.0"] : xs'

