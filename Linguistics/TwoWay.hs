
module Linguistics.TwoWay
  ( twoWayBigram
  , twoWaySimple
  ) where

import qualified Linguistics.TwoWay.Bigram as Bigram
import qualified Linguistics.TwoWay.Simple as Simple

twoWayBigram = Bigram.twoWay
twoWaySimple = Simple.twoWay

