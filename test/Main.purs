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

  let test = lookup 2 l1 -- Just 12
  let testFailing = lookupOriginal 2 l1 -- Nothing

  Assert.assertEqual { expected: Just 12, actual: lookup 2 l1 }
  log "passed #1"
  Assert.assertEqual { expected: Just 12, actual: lookupOriginal 2 l1 }
  log "passed #2"
