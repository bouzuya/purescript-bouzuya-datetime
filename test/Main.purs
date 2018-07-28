module Test.Main (main) where

import Effect (Effect)
import Prelude (Unit)
import Test.DayOfYear as DayOfYear
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  DayOfYear.tests
