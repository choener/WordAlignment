
-- | Creates producers for efficient enumeration of all language pairs and
-- for each language pair for all words. All words are either all @k * l@
-- words for two different languages, or the upper triangular matrix, if
-- a language is paired with itself.

module Linguistics.WordAlignment.PipedPairs where

import           Control.Arrow (second)
import           Control.DeepSeq
import           Control.Lens hiding (each)
import           Control.Monad.State.Strict
import           Data.ByteString.Builder (Builder)
import           Data.Default
import           Data.Function (on)
import           Data.List (groupBy,delete,partition)
import           Data.Vector (Vector)
import           Debug.Trace
import           Pipes hiding ((<~))
import           Prelude hiding (Word)
import qualified Data.Vector as V
import qualified Pipes.Prelude as PP
import           System.Mem (performGC)

import           Data.Paired.Vector
import           NLP.Text.BTI

import           Linguistics.WordAlignment.FastLookups
import           Linguistics.WordAlignment.Word (Word, wordLang, FastChars, fastChars)



-- | Alignment configuration for use in the Reader.

data AlignmentConfig t = AlignmentConfig
  { _aliWidth             :: !Int               -- ^ width of columns for the builder
  , _aliNumCoopts         :: !Int               -- ^ how many co-optimals to take
  , _aliGroups            :: !Int               -- ^ how many groups will be processed
  , _aliCurGroup          :: !Int               -- ^ current group
  , _aliFilterScore       :: !(Maybe Double)    -- ^ if @Just s@ then keep only scores @>= s@
  , _aliFilterBackt       :: !(Maybe Double)    -- ^ if @Just s@ then backtrack only for scores @>= s@
  , _aliFilterNormalized  :: !Bool              -- ^ apply filter on length-normalized scores
  , _aliFilterLanguages   :: ![Either Int BTI]  -- ^ if @not null@ then we want only the languages indicated here. @Left Int@ is the language index, @Right BTI@ the language name
  , _aliVerbose           :: !Bool              -- ^ be verbose (what that means depends on the functions)
  , _aliGroupLanguages    :: ![BTI]             -- ^ the languages used in the current group
  , _aliCustom            :: !t                 -- ^ custom data
  }

instance Default t => Default (AlignmentConfig t) where
  def = AlignmentConfig
          { _aliWidth = 8
          , _aliNumCoopts = 1
          , _aliGroups = 0
          , _aliCurGroup = 0
          , _aliFilterScore = Nothing
          , _aliFilterBackt = Nothing
          , _aliFilterNormalized = False
          , _aliFilterLanguages = []
          , _aliVerbose = False
          , _aliGroupLanguages = []
          , _aliCustom = def
          }

makeLenses ''AlignmentConfig



-- | Generic system for twoway alignments.
--
-- TODO this will almost surely force the @ps@ vector, which maybe is not
-- what we want.

runTwowayAlignments
  :: (Monad m)
  -- | Perform an action before each group is sent downstream
  => (Int -> Vector (Word,Word) -> AlignmentT t m t)
  -- | Perform the actual alignment
  -> (t -> FastChars -> FastDoubles -> Word -> Word -> AlignmentT t m Builder)
  -- | An action that might do something with each total length, pair
  -- counter, pairX, pairY tripel.
  -> (Int -> Int -> Word -> Word -> AlignmentT t m ())
  -- | Fast Doubles
  -> [Double]
  -- | The input words
  -> Vector Word
  -- | A producer of 'Builder's
  -> Producer Builder (AlignmentT t m) ()
runTwowayAlignments groupAction alignXY eachXY ds ws = do
  width <- use aliWidth
  let !fc = fastChars width ws
  let !fd = fastDoubles width ds
  -- Generate the pairs of languages
  for (languagePairProducer ws) $ \(lenPs,ps) -> do
    !t <- lift $ groupAction lenPs ps
    for (each $ V.indexed ps) $ \(k,(x,y)) -> do
      lift $ eachXY lenPs k x y
      lift (alignXY t fc fd x y) >>= yield
{-# Inline runTwowayAlignments #-}

-- | Given the full list of words in the vector @ws@, produce all pairs of
-- words to be aligned.
--
-- This function will produce an upper-triangular list if the two languages
-- are the same, since @(x,y)@ and @(y,x)@ are then the same in enumeration
-- due to @x,y ε S@. For different languages we have @x ε X@ and @y ε Y@
-- and need the full square set. This is implemented below.
--
-- @ws@ is evaluated to NF.
--
-- TODO re-add language filter!

languagePairProducer
  :: (Monad m)
  => Vector Word
  -> Producer (Int, Vector (Word,Word)) (AlignmentT t m) ()
languagePairProducer ws = do
  -- produces a vector with (wordLanguage, language index from 1, first index in @ws@, length of
  -- group)
  let gs = deepseq ws . V.fromList . map (\(k,g@((i,w):_)) -> (wordLang w,k,i,length g))
         . zip [1..] . groupBy ((==) `on` (wordLang . snd)) . V.toList . V.indexed $ ws
  -- from @gs@ we generate the upper-triangular pairs
  let (lenWgs,wgs) = second (V.map mkGroup) . upperTriVG OnDiag $ gs
  aliGroups .= lenWgs
  -- each wgs
  -- NOTE it is imperative that @wgs@ be fusioned away, otherwise this will
  -- generate a very huge in-memory representation of all input pairs.
  for (each $ V.indexed wgs) $
    \(k,w) -> do
      aliCurGroup .= k
      aliGroupLanguages .= w ^. _1
      afl <- use aliFilterLanguages
      let cls = zip (w^._1) (w^._2)
      when (null afl || accept afl cls) $
        yield $ w ^. _3
  where
    -- This builds up a group
    -- TODO the lang and index lists should be @nub@'bed, so that @accept@
    -- can actually drop acceptance criteria after testing?!
    mkGroup ( (langX,lixX,startX,lengthX) , (langY,lixY,startY,lengthY) )
      -- same language, perform upper-triangular number of comparisons.
      -- Test for starting position as well, in case we have non-contiguous
      -- data.
      | langX == langY && startX == startY
      = ([langX], [lixX], upperTriVG OnDiag (V.slice startX lengthX ws))
      | otherwise
      = ([langX,langY], [lixX,lixY], rectangularVG (V.slice startX lengthX ws) (V.slice startY lengthY ws))
    -- here, @accept []@ should actually be false now, since we test
    -- a non-empty list of acceptance conditions
    accept [] [] = True -- all languages found in constraints
    accept (Left  k : as) cs | ([_], cs') <- partition ((k==) . snd) cs = accept as cs'
    accept (Right l : as) cs | ([_], cs') <- partition ((l==) . fst) cs = accept as cs'
    accept _ _ = False
{-# Inline languagePairProducer #-}

-- | All important information for running the alignment producer.

newtype AlignmentT t (m :: * -> *) a = AlignmentT
  { runAlignmentT :: StateT (AlignmentConfig t) m a
  }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadState (AlignmentConfig t)
    , MonadTrans
    )

-- | Wrap up the full call to the monad transformer

runAlignment
  :: Monad m
  => Effect (AlignmentT t m) a -> AlignmentConfig t -> m a
runAlignment = evalStateT . runAlignmentT . runEffect
{-# Inline runAlignment #-}

-- | Useful default group action: perform garbage collection after a group
-- has been processed.

groupActionGC _ _ = lift performGC
{-# Inline groupActionGC #-}

