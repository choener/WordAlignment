
module Linguistics.FourWay
  ( fourWaySimple
  , fourWayBigram
  ) where

import qualified Linguistics.FourWay.Simple as Simple
--import qualified Linguistics.FourWay.Bigram as Bigram

fourWaySimple = Simple.fourWay
fourWayBigram = error "missing" -- Bigram.fourWay

