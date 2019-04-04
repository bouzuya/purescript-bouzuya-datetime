module Test.Main (main) where

import Effect (Effect)
import Prelude (Unit, discard)
import Test.Bouzuya.DateTime.Date.Extra as DateExtra
import Test.Bouzuya.DateTime.Date.YearMonth as DateYearMonth
import Test.Bouzuya.DateTime.OffsetDateTime as OffsetDateTime
import Test.Bouzuya.DateTime.OrdinalDate as OrdinalDate
import Test.Bouzuya.DateTime.OrdinalDate.Component.DayOfYear as OrdinalDateComponentDayOfYear
import Test.Bouzuya.DateTime.TimeZoneOffset as TimezoneOffset
import Test.Bouzuya.DateTime.WeekDate as WeekDate
import Test.Bouzuya.DateTime.WeekDate.Component.Week as WeekDateComponentWeek
import Test.Bouzuya.DateTime.WeekDate.Component.WeekYear as WeekDateComponentWeekYear
import Test.Bouzuya.DateTime.WeekDate.Extra as WeekDateExtra
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  DateExtra.tests
  DateYearMonth.tests
  OffsetDateTime.tests
  OrdinalDate.tests
  OrdinalDateComponentDayOfYear.tests
  TimezoneOffset.tests
  WeekDate.tests
  WeekDateComponentWeek.tests
  WeekDateComponentWeekYear.tests
  WeekDateExtra.tests
