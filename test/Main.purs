module Test.Main (main) where

import Effect (Effect)
import Prelude (Unit, discard)
import Test.DayOfYear as DayOfYear
import Test.OrdinalDate as OrdinalDate
import Test.Unit.Main (runTest)
import Test.WeekOfYear as WeekOfYear

main :: Effect Unit
main = runTest do
  DayOfYear.tests
  OrdinalDate.tests
  WeekOfYear.tests
