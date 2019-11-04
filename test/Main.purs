module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.Assert as Assert
import Data.SkewList
import Data.Maybe

main :: Effect Unit
main = do
  let l1 = cons 10 $ cons 11 $ cons 12 $ cons 13 nil

  Assert.assertEqual { expected: Just 12, actual: lookup 2 l1 }
  Assert.assertEqual { expected: 4, actual: length l1 }
