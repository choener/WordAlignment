
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

import           Data.Vector.Combined

import qualified Linguistics.WordAlignment.TwoWay.Infix.Simple as IS2
import qualified Linguistics.WordAlignment.TwoWay.Infix.Bigram as IB2
import           Linguistics.WordAlignment.AlignmentBuilder
import           Linguistics.WordAlignment.Word (Word, wordLang, FastChars, fastChars)



alignInfixSimple2 = IS2.alignInfix
alignInfixBigram2 = IB2.alignInfix



-- | Alignment configuration for use in the Reader.

data AlignmentConfig t = AlignmentConfig
  { _aliWidth     :: !Int   -- ^ width of columns for the builder
  , _aliGroups    :: !Int   -- ^ how many groups will be processed
  , _aliCurGroup  :: !Int   -- ^ current group
  , _aliCustom    :: !t     -- ^ custom data
  }

instance Default t => Default (AlignmentConfig t) where
  def = AlignmentConfig
          { _aliWidth = 8
          , _aliGroups = 0
          , _aliCurGroup = 0
          , _aliCustom = def
          }

makeLenses ''AlignmentConfig



-- | Generic system for twoway alignments.

runTwowayAlignments
  :: (Monad m)
  -- | Perform an action before each group is sent downstream
  => (Int -> Vector (Word,Word) -> AlignmentT t m t)
  -- | Perform the actual alignment
  -> (t -> FastChars -> Word -> Word -> AlignmentT t m Builder)
  -- | An action that might do something with each total length, pair
  -- counter, pairX, pairY tripel.
  -> (Int -> Int -> Word -> Word -> AlignmentT t m ())
  -- | The input words
  -> Vector Word
  -- | A producer of 'Builder's
  -> Producer Builder (AlignmentT t m) ()
runTwowayAlignments groupAction alignXY eachXY ws = do
  width <- use aliWidth
  let !fc = fastChars width ws
  -- Generate the pairs of languages
  for (languagePairProducer ws) $ \(lenPs,ps) -> do
    !t <- lift $ groupAction lenPs ps
    for (each $ V.indexed ps) $ \(k,(x,y)) -> do
      lift $ eachXY lenPs k x y
      lift (alignXY t fc x y) >>= yield
{-# Inline runTwowayAlignments #-}

-- |
--
-- TODO re-add language filter!

languagePairProducer
  :: (Monad m)
  => Vector Word
  -> Producer (Int, Vector (Word,Word)) (AlignmentT t m) ()
languagePairProducer ws = do
  -- produces a vector with (wordLanguage, first index in @ws@, length of
  -- group)
  let gs = V.fromList . map (\g@((i,w):_) -> (wordLang w,i,length g))
         . groupBy ((==) `on` (wordLang . snd)) . V.toList . V.indexed $ ws
  -- from @gs@ we generate the upper-triangular pairs
  let (lenWgs,wgs) = second (V.map mkGroup) . upperTriVG OnDiag $ gs
  aliGroups .= lenWgs
  -- each wgs
  for (each $ V.indexed wgs) $ \(k,w) -> aliCurGroup .= k >> yield w
  where
    -- This builds up a group
    mkGroup ( (langX,startX,lengthX) , (langY,startY,lengthY) )
      -- same language, perform upper-triangular number of comparisons.
      -- Test for starting position as well, in case we have non-contiguous
      -- data.
      | langX == langY && startX == startY
      = upperTriVG OnDiag (V.slice startX lengthX ws)
      | otherwise
      = rectangularVG (V.slice startX lengthX ws) (V.slice startY lengthY ws)
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

