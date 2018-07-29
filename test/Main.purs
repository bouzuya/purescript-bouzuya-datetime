module Test.Main (main) where

import Effect (Effect)
import Prelude (Unit, discard)
import Test.DayOfYear as DayOfYear
import Test.Unit.Main (runTest)
import Test.WeekOfYear as WeekOfYear

main :: Effect Unit
main = runTest do
  DayOfYear.tests
  WeekOfYear.tests
