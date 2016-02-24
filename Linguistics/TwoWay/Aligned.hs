
-- | Efficient encoding of alignments.

module Linguistics.TwoWay.Aligned where

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.List (intersperse)
import           Data.Monoid
import           Data.Set (Set)
import           GHC.Generics
import           Prelude hiding (Word)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Set as S

import Linguistics.Word



data Alignment = Alignment
  { xId   :: Int
  , yId   :: Int
  , score :: Double
  }
  deriving (Show,Generic)

instance ToJSON Alignment where
  toJSON (Alignment x y s) = object
    [ "x"      .= x
    , "y"      .= y
    , "score"  .= s
    ]

data AlignedSet = AlignedSet
  { alignedWords :: [Word]
  , alignments   :: [Alignment]
  }
  deriving (Show,Generic)

instance ToJSON AlignedSet where
  toJSON (AlignedSet ws as) = object
    [ "alignedWords" .= ws
    , "alignments"   .= as
    ]

encodeAlignedSet
  :: [(Word,Word)]
  -> [(Int,Int,Double)]
  -> ByteString
encodeAlignedSet ws' as = BB.toLazyByteString $ ews <> "\n" <> eas <> "\n"
  where ws = S.toList . S.fromList $ concatMap (\(x,y) -> [x,y]) ws'
        ews = mconcat . intersperse "\n" . map (BB.lazyByteString . encode) $ ws
        eas = mconcat . intersperse "\n" . map (BB.lazyByteString . encode) $ as

mkAlignedSet
  :: [(Word,Word)]  -- | The words we aligned
  -> [(Int,Int,Double)]
  -> AlignedSet
mkAlignedSet ws as = AlignedSet
  { alignedWords = S.toList . S.fromList $ concatMap (\(x,y) -> [x,y]) ws
  , alignments   = map (\(x,y,s) -> Alignment x y s) as
  }

