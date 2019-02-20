module Test.Main (main) where

import Effect (Effect)
import Prelude (Unit, discard)
import Test.OrdinalDate as OrdinalDate
import Test.OrdinalDate.Component.DayOfYear as OrdinalDateComponentDayOfYear
import Test.Unit.Main (runTest)
import Test.WeekOfYear as WeekOfYear

main :: Effect Unit
main = runTest do
  OrdinalDate.tests
  OrdinalDateComponentDayOfYear.tests
  WeekOfYear.tests
