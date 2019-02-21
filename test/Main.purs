module Test.Main (main) where

import Effect (Effect)
import Prelude (Unit, discard)
import Test.Bouzuya.OrdinalDate as OrdinalDate
import Test.Bouzuya.OrdinalDate.Component.DayOfYear as OrdinalDateComponentDayOfYear
import Test.Bouzuya.WeekDate as WeekDate
import Test.Bouzuya.WeekDate.Component.Week as WeekDateComponentWeek
import Test.Bouzuya.WeekDate.Component.WeekYear as WeekDateComponentWeekYear
import Test.Bouzuya.WeekDate.Extra as WeekDateExtra
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  OrdinalDate.tests
  OrdinalDateComponentDayOfYear.tests
  WeekDate.tests
  WeekDateComponentWeek.tests
  WeekDateComponentWeekYear.tests
  WeekDateExtra.tests
