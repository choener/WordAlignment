
{-# Options_GHC -fno-cse #-}

module Main where

import           Control.Arrow ((***),(&&&))
import           Control.Concurrent (threadDelay)
import           Control.Monad (forM_,when)
import           Control.Parallel.Strategies (rdeepseq,parMap,parBuffer,using,evalTuple2,r0,rseq,evalBuffer,parList,evalList,evalTuple3,evalTuple5)
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
import qualified System.Console.AsciiProgress as CAP
import           System.Console.CmdArgs
import           System.Exit
import           System.IO
import           System.Mem (performGC)
import           Text.Printf
import           Text.Read (readMaybe)
import           Data.Stringable (toString)

import           NLP.Scoring.SimpleUnigram
import           NLP.Scoring.SimpleUnigram.Import
import           NLP.Text.BTI

import           Linguistics.Bigram
import           Linguistics.Common
import           Linguistics.TwoWay.Simple
import           Linguistics.Word (parseWord,Word(..),addWordDelims)
import qualified Linguistics.TwoWay.Bigram as BI

import           Paths_WordAlignment (version)



data Config
  = TwoWaySimple
    { scoreFile     :: String
    , lpblock       :: Maybe (String,String)
    , showManual    :: Bool
    , prettystupid  :: Bool
    , outfile       :: String
    , nobacktrack   :: Bool
    }
  | TwoWay
    { scoreFile     :: String
    , bigramDef     :: Double
    , gapOpen       :: Double
    , lpblock       :: Maybe (String,String)
    , showManual    :: Bool
    , prettystupid  :: Bool
    , outfile       :: String
    , nobacktrack   :: Bool
    , serialized    :: Bool
    }
  deriving (Show,Data,Typeable)

twowaySimple = TwoWaySimple
  { scoreFile  = def &= help "the file to read the simple scores from"
  , lpblock    = Nothing
  , showManual = False    &= help "show the manual and quit"
  , prettystupid = False
  , outfile      = ""
  , nobacktrack   = False
  } &= help "Align words based on a simple, linear scoring model for gaps, and an unigram model for matches."

twoway = TwoWay
  { scoreFile  = "" &= help "the file to read the scores from"
  , bigramDef  = (-20) &= help "score to use for unknown bigram matches"
  , gapOpen    = (-5) &= help "cost to open a gap"
  , lpblock    = Nothing  &= help "compare ONLY the given pair of languages: i.e 'Breton','Breton' or 2,3  (with the latter notation '2' being the 2nd language in the input file)"
  , prettystupid = False  &= help "a pretty stupid developer option"
  , outfile      = ""     &= help "write output to this file"
  , showManual = False
  , nobacktrack   = False
  , serialized = False
  } &= help "Align words based on a linear scoring model for gaps, but with bigram-based scoring for matches."

config = [twowaySimple, twoway]
  &= program "WordAlign"
  &= summary ("WordAlign " ++ showVersion version ++ " (c) Christian Höner zu Siederdissen 2014--2015, choener@bioinf.uni-leipzig.de")

embeddedManual = $(embedFile "README.md")



main = do
  o <- cmdArgs $ modes config
  when (showManual o) $ do
    BS.putStrLn embeddedManual
    exitSuccess
  when (prettystupid o && null (outfile o)) $ do
    putStrLn "The --prettystupid mode requires giving and --outfile"
    exitFailure
  (if prettystupid o then CAP.displayConsoleRegions else id) $ do
    ws <- BL.getContents >>= return . map parseWord . BL.lines
    case o of
      TwoWaySimple{..} -> run2Simple o (blockSelection2 lpblock ws)
      TwoWay{..}       -> run2 o (blockSelection2 lpblock $ map addWordDelims ws)



-- | Given a @Config@ and a @List of Lists of Word-Pairs@ align everything.

run2Simple :: Config -> WSS -> IO ()
run2Simple TwoWaySimple{..} wss = do
  hndl <- if null outfile then return stdout else openFile outfile AppendMode
  scoring <- simpleScoreFromFile scoreFile
  let wsslen = length wss
  -- for each language pairing
  forM_ (zip [1::Int ..] wss) $ \(k,ws) -> do
    let len = genericLength ws
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
      when (prettystupid && k `mod` 10000 == 0) $ printf "%s %s %10d %10d\n" wLx wLy (len::Int) k
      TL.hPutStr hndl sss
      --when (isJust pg) $ let Just pg' = pg in CAP.tick pg'

-- | Given a @Config@ and a @List of List of Word-Pairs@ align everything.

run2 :: Config -> WSS -> IO ()
run2 TwoWay{..} wss = {-# SCC "run2" #-} do
  hndl <- if null outfile then return stdout else openFile outfile AppendMode
  let wsslen = length wss
  -- build up scoring system
  let chkLs = S.fromList . map wordLang . concat . map (\(x,y) -> [x,y]) . map head $ wss
  scoring <- BL.readFile scoreFile >>= return . generateLookups chkLs (-999999)
  -- for each language pairing
  forM_ (zip [1::Int ..] wss) $ \(k,ws) -> do
    let len = genericLength ws
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
    -- get score pairing
    let (hx,hy) = head ws
    let sco = {-# SCC "run2/sco" #-} getScores2 scoring (wordLang hx) (wordLang hy)
    if not serialized
    then do
      -- align the words the in @ws@ pairing
      let as = {-# SCC "run2/as" #-}
                map (\(x,y) -> ( let (d,bts) = BI.alignGlobal bigramDef gapOpen sco 1 (wordWord x) (wordWord y)
                                 in  TL.toLazyText $ buildAlignment (-1) ([x,y],(d,if nobacktrack then [] else bts))
                               )
                    ) ws
      forM_ (zip [1::Int ..] as) $ \(!k,!ali) -> do
--        when (k `mod` 1000 == 0) $ maybe (return ()) (`CAP.tickN` 1000) pg
        when (prettystupid && k `mod` 10000 == 0) $ printf "%s %s %10d %10d\n" wLx wLy (len::Int) k
        TL.hPutStr hndl ali
--      maybe (return ()) CAP.complete pg
    else do
      let as = {-# SCC "run2/ser" #-}
                map (\(x,y) -> ( let (d,_) = BI.alignGlobal bigramDef gapOpen sco 0 (wordWord x) (wordWord y)
                                 in  (wordID x, wordID y, d)
                               )
                    ) ws
      print as

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

blockSelection2 :: Maybe (String,String) -> [Word] -> WSS
blockSelection2 s ws = {-# SCC "blockSelection2" #-} go (mkCmp s)
        -- grouping words by their languages, pair each language group with
        -- an index
  where gs = zip [1..] $ groupBy ((==) `on` wordLang) ws
        -- Gives a map "Language String Name" -> Int (for the gs)
        ls = M.fromList $ map (\(k,(v:_)) -> (show $ wordLang v,k)) gs
        -- produces the word pairs to be aligned
        go f = [ [ (x,y)
                 | (kk,x) <- zip [1..] xs, (ll,y) <- zip [1..] ys   -- actually enumerate the words
                 , k/=l || kk < ll                                  -- upper-tri for same group, otherwise all alignments
                 ]
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

type WSS = [[(Word,Word)]] -- V.Vector (V.Vector (Word,Word))

-- | (write me)

getScores2 :: Mapping -> Lang -> Lang -> Scores
getScores2 ss a b
  | Just z <- M.lookup (a:!:b) (lliid ss) = z
  | otherwise = trace (printf "Language pair %s %s not found in mapping! Returning empty hashmap\n" (toUtf8String a) (toUtf8String b))
                HM.empty

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

buildAlignmentSimple :: Double -> ([Linguistics.Word.Word],(Double,[[[Text]]])) -> TL.Builder
buildAlignmentSimple k (ws,(s,(xs))) = TL.fromText hdr `mappend` ls `mappend` "\n" where
  ids = concat . intersperse " " . map (show . wordID)   $ ws
  wds = concat . intersperse "   WORD   " . map (concat . intersperse " " . map toUtf8String . VU.toList . wordWord) $ ws
  ns = s / (maximum $ 1 : map ((+k) . fromIntegral . VU.length . wordWord) ws)
  hdr = T.pack $ printf "IDS: %s SCORE: %.2f NSCORE: %.2f    WORDS: %s\n" ids s ns wds
  ls  = case xs of [] -> "" ; [xs'] -> buildLines xs'

buildAlignment :: Double -> ([Linguistics.Word.Word],(Double,[[[Text]]])) -> TL.Builder
buildAlignment k (ws,(s,(xss))) = {-# SCC "pretty_ali" #-} TL.fromText hdr `mappend` ls `mappend` "\n" where
  ids = concat . intersperse " " . map (show . wordID)   $ ws
  wds = concat . intersperse "   WORD   " . map (concat . intersperse " " . map toUtf8String . VU.toList . wordWord) $ ws
  ns = s / (maximum $ 1 : map ((+k) . fromIntegral . VU.length . wordWord) ws)
  hdr = T.pack $ printf "IDS: %s SCORE: %.2f NSCORE: %.2f    WORDS: %s\n" ids s ns wds
  ls  = case xss of [] -> "" ; [xs'] -> buildLines $ ["^","^","0.0"] : xs'
























{-
import           Control.Arrow
import           Control.Monad (unless)
import           Control.Monad (when)
import           Control.Parallel.Strategies
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Function
import           Data.List (intersperse)
import           Data.List (tails,genericLength,genericTake,genericDrop,group)
import           Data.Ord
import           Data.Strict.Tuple
import           Data.Vector (fromList)
import           Debug.Trace (trace)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.HashTable.IO as H
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import           System.Console.CmdArgs
import           System.IO.Unsafe (unsafePerformIO)
import           Text.Printf

import           NLP.Alphabet.MultiChar
import           NLP.Scoring.SimpleUnigram
import           NLP.Scoring.SimpleUnigram.Default
import           NLP.Scoring.SimpleUnigram.Import

import Linguistics.TwoWay.Simple
{- we test interning with TwoWay alignments only
import Linguistics.ThreeWay
import Linguistics.FourWay
-}
import Linguistics.Bigram
import Linguistics.Common
import Linguistics.Word
import Linguistics.TwoWay.AdvancedBigram

data Config
  = TwoWay
    { scoreFile :: String
    , bigramDef :: Double
    , unibiDef  :: Double
    , gapOpen :: Double
    , gapExtend :: Double
    , block :: Maybe (Integer,Integer)
    , selfAlign :: Bool
    }
  | TwoWaySimple
    { scoreFile :: String
    , block :: Maybe (Integer,Integer)
    , selfAlign :: Bool
    }
  {- NLA
  | ThreeWay
    { scoreFile :: String
    , defaultScore :: Double
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  | ThreeWaySimple
    { scoreFile :: String
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  | FourWay
    { scoreFile :: String
    , defaultScore :: Double
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  | FourWaySimple
    { scoreFile :: String
    , gapOpen :: Double
    , block :: Maybe (Integer,Integer)
    }
  -}
  | Info
    {
    }
  deriving (Show,Data,Typeable)

twoway = TwoWay
  { scoreFile = "" &= help "the file to read the scores from"
  , bigramDef = (-20) &= help "score to use for unknown bigram matches"
  , unibiDef  = (-5) &= help "score to close a gap if the closing characters are unknown"
  , gapOpen = (-5) &= help "cost to open a gap"
  , gapExtend = (-1) &= help "cost to extend a gap"
  , block = Nothing &= help "when using --block N,k calculate only the k'th block (starting at 1) with length N. For parallelized computations."
  , selfAlign = False &= help "align each word with itself as well"
  } &= help "Align two words at a time for all ordered word combinations"

twowaySimple = TwoWaySimple
  { scoreFile = def &= help ""
  }

{- NLA
threeway = ThreeWay
  {
  }

threewaySimple = ThreeWaySimple
  {
  }

fourway = FourWay
  {
  }

fourwaySimple = FourWaySimple
  {
  }
-}

info = Info
  {
  }

config = [twoway,twowaySimple, {- threeway,threewaySimple,fourway,fourwaySimple, -} info]
  &= program "WordAlign"
  &= summary "WordAlign v.0.0.1"

main = do
  o <- cmdArgs $ modes config
  ws' <- BL.getContents >>= return . map parseWord . BL.lines
  case o of
    Info{} -> do
      let l :: Integer = genericLength ws'
      let c2 = l * (l-1) `div` 2 -- number of alignments
      let t2 :: Double = fromIntegral c2 / 5000 / 60 / 60 -- approximate time in hours
      let c3 = l * (l-1) * (l-2) `div` 3
      let t3 :: Double = fromIntegral c3 / 5000 / 60 / 60 -- TODO fix time constant
      let c4 = l * (l-1) * (l-2) * (l-3) `div` 4
      let t4 :: Double = fromIntegral c4 / 5000 / 60 / 60 -- TODO fix time constant
      printf "%d  %.1f    %d  %.1f    %d  %.1f\n" c2 t2 c3 t3 c4 t4
    TwoWay{..} -> do
      let ws = map addWordDelims ws'
      let bs = blockWith block $ [ (a,b) | (a:as) <- tails ws, b <- if selfAlign then (a:as) else as ]
      let chkLs = S.fromList . map wordLang $ ws
      {-
      let chkLs = if block==Nothing
                    then S.fromList . map wordLang $ ws
                    else S.fromLIst . map wordLang $ ws -- S.fromList . map head . group . map wordLang . concatMap (\(a,b) -> [a,b]) $ bs
                    -}
      ss <- BL.readFile scoreFile >>= return . generateLookups chkLs (-999999)
      let ts = map (\(a,b) -> ( [a,b], alignTwo (BigramScores {gapOpen = gapOpen, gapExtend = gapExtend, bigramDef = bigramDef, unigramDef = -999999, biUniDef = unibiDef, scores = getScores2 ss (wordLang a) (wordLang b)})
                                                (wordWord a) (wordWord b)
                              )
                    ) bs
      mapM_ (printAlignment (-2)) ts
    TwoWaySimple{..} -> do
      simpleScoring <- if null scoreFile then return $ error "scorefile missing"
                                         else simpleScoreFromFile scoreFile
      let ws = ws'
      let bs = blockWith block $ [ (a,b) | (a:as) <- tails ws, b <- if selfAlign then (a:as) else as ]
      let ts = map (\(a,b) -> ( [a,b], alignTwoSimple simpleScoring (wordWord a) (wordWord b)
                              )
                    ) bs
      mapM_ (printAlignment 0) ts
    {-
    ThreeWay{..} -> do
      let ws = map addWordDelims ws'
      let bs = blockWith block $ [ (a,b,c) | (a:as) <- tails ws, (b:bs) <- tails as, c <- bs ]
      let chkLs = S.fromList . map wordLang $ ws
      {-
      let chkLs = if block==Nothing
                    then S.fromList . map wordLang $ ws
                    --else S.fromList . map head . group . map wordLang . concatMap (\(a,b,c) -> [a,b,c]) $ bs
                    else S.fromLIst . map wordLang $ ws -- S.fromList . map head . group . map wordLang . concatMap (\(a,b) -> [a,b]) $ bs
                    -}
      ss <- BL.readFile scoreFile >>= return . generateLookups chkLs defaultScore
      let ts = ss `seq` map (\(a,b,c) -> ( [a,b,c], alignThree defaultScore gapOpen (getScores3 ss (wordLang a) (wordLang b) (wordLang c))
                                                (wordWord a) (wordWord b) (wordWord c)
                                         )
                            ) bs
      mapM_ (printAlignment (-2)) ts
    ThreeWaySimple{..} -> do
      [vwl,cns] <- readFile vowelConsonantFile >>= return . map VU.fromList . lines
      scs <- if null scoreFile then return [3,1,1,0,0,-1] else (readFile scoreFile >>= return . map read . words)
      let ws = ws'
      let bs = blockWith block $ [ (a,b,c) | (a:as) <- tails ws, (b:bs) <- tails as, c <- bs ]
      let ts = map (\(a,b,c) -> ( [a,b,c], alignThreeSimple vwl cns scs gapOpen (wordWord a) (wordWord b) (wordWord c)
                                )
                    ) bs
      mapM_ (printAlignment 0) ts
    FourWay{..} -> do
      let ws = map addWordDelims ws'
      let bs = blockWith block $ [ (a,b,c,d) | (a:as) <- tails ws, (b:bs) <- tails as, (c:cs) <- tails bs, d <- cs ]
      let chkLs = S.fromList . map wordLang $ ws
      {-
      let chkLs = if block==Nothing
                    then S.fromList . map wordLang $ ws
                    --else S.fromList . map head . group . map wordLang . concatMap (\(a,b,c,d) -> [a,b,c,d]) $ bs
                    else S.fromLIst . map wordLang $ ws -- S.fromList . map head . group . map wordLang . concatMap (\(a,b) -> [a,b]) $ bs
                    -}
      ss <- BL.readFile scoreFile >>= return . generateLookups chkLs defaultScore
      let ts = map (\(a,b,c,d) -> ( [a,b,c,d], alignFour defaultScore gapOpen (getScores4 ss (wordLang a) (wordLang b) (wordLang c) (wordLang d))
                                                (wordWord a) (wordWord b) (wordWord c) (wordWord d)
                                  )
                    ) bs
      mapM_ (printAlignment (-2)) ts
    FourWaySimple{..} -> do
      [vwl,cns] <- readFile vowelConsonantFile >>= return . map VU.fromList . lines
      scs <- if null scoreFile then return [3,1,1,0,0,-1] else (readFile scoreFile >>= return . map read . words)
      let ws = ws'
      let bs = blockWith block $ [ (a,b,c,d) | (a:as) <- tails ws, (b:bs) <- tails as, (c:cs) <- tails bs, d <- cs ]
      let ts = map (\(a,b,c,d) -> ( [a,b,c,d], alignFourSimple vwl cns scs gapOpen (wordWord a) (wordWord b) (wordWord c) (wordWord d)
                                  )
                    ) bs
      mapM_ (printAlignment 0) ts
      -}

alignTwo :: BigramScores -> V.Vector InternedMultiChar -> V.Vector InternedMultiChar -> (Double, [[String]])
alignTwo scores x y
  = second (map alignPretty) -- . filter (any (\c -> c/= "$" && c/= "^")))
  $ runBigram scores 1 x y

alignTwoSimple
  :: SimpleScoring
  -> V.Vector InternedMultiChar
  -> V.Vector InternedMultiChar
  -> (Double, [[String]])
alignTwoSimple simpleScoring x y = second (map alignPretty) $ twoWaySimple simpleScoring x y

{-
alignThree :: Double -> Double -> (Scores,Scores,Scores) -> V.Vector ByteString -> V.Vector ByteString -> V.Vector ByteString -> (Double, [[String]])
alignThree sDef sGapOpen scores x y z = second (map (alignPretty . map (filter (\c -> c/= "$" && c/="^")) . tup3List)) $ threeWayBigram sDef sGapOpen scores x y z

alignThreeSimple
  :: VU.Vector Char
  -> VU.Vector Char
  -> [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> (Double, [[String]])
alignThreeSimple v c scores sGapOpen x y z = second (map (alignPretty . tup3List)) $ threeWaySimple v c scores sGapOpen x y z

alignFour :: Double -> Double -> (Scores,Scores,Scores,Scores,Scores,Scores) -> V.Vector ByteString -> V.Vector ByteString -> V.Vector ByteString -> V.Vector ByteString -> (Double, [[String]])
alignFour sDef sGapOpen scores w x y z = second (map (alignPretty . map (filter (\c -> c/= "$" && c/="^")) . tup4List)) $ fourWayBigram sDef sGapOpen scores w x y z

alignFourSimple
  :: VU.Vector Char
  -> VU.Vector Char
  -> [Double]
  -> Double
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> V.Vector ByteString
  -> (Double, [[String]])
alignFourSimple v c scores sGapOpen w x y z = second (map (alignPretty . tup4List)) $ fourWaySimple v c scores sGapOpen w x y z
-}

blockWith Nothing      xs = xs
blockWith (Just (l,k)) xs = genericTake l . genericDrop (l * (k-1)) $ xs

getScores2 :: Mapping -> Lang -> Lang -> Scores
getScores2 ss a b
  | Just z <- M.lookup (a:!:b) (lliid ss) = z
  | otherwise = trace (printf "Language pair %s %s not found in mapping! Returning empty hashmap\n" (toUtf8String a) (toUtf8String b))
                (unsafePerformIO H.new)

{-
getScores3 :: Mapping -> Lang -> Lang -> Lang -> (Scores,Scores,Scores)
getScores3 ss a b c = (getScores2 ss a b, getScores2 ss a c, getScores2 ss b c)  -- (lliid ss M.! (a:!:b), lliid ss M.! (a:!:c), lliid ss M.! (b:!:c))

getScores4 :: Mapping -> Lang -> Lang -> Lang -> Lang -> (Scores,Scores,Scores,Scores,Scores,Scores)
getScores4 ss a b c d = (getScores2 ss a b, getScores2 ss a c, getScores2 ss a d, getScores2 ss b c, getScores2 ss b d, getScores2 ss c d)
-- (lliid ss M.! (a:!:b), lliid ss M.! (a:!:c), lliid ss M.! (a:!:d), lliid ss M.! (b:!:c), lliid ss M.! (b:!:d), lliid ss M.! (c:!:d))
-}

printAlignment :: Double -> ([Word], (Double, [[String]])) -> IO ()
printAlignment k (ws,(s,[])) = do
  printf "DEBUG!\nScore: %f\nDEBUG!\n\n" s

printAlignment k (ws,(s,(x:xs))) = do
  let ids = concat . intersperse " " . map (show . wordID)   $ ws
  let wds = concat . intersperse "   WORD   " . map (concat . intersperse " " . map toUtf8String . V.toList . wordWord) $ ws
  let ns = s / (maximum $ 1 : map ((+k) . fromIntegral . V.length . wordWord) ws)
  printf "IDS: %s SCORE: %.2f NSCORE: %.2f    WORDS: %s\n" ids s ns wds
  mapM_ putStrLn x
  putStrLn ""


tup2List (a,b) = [a,b]
tup3List (a,b,c) = [a,b,c]
tup4List (a,b,c,d) = [a,b,c,d]

-}

