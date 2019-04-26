module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Bouzuya.DateTime.Date.Interval.Year as DateIntervalYear
import Test.Bouzuya.DateTime.Date.Interval.YearMonth as DateIntervalYearMonth
import Test.Bouzuya.DateTime.OffsetDateTime as OffsetDateTime
import Test.Bouzuya.DateTime.OrdinalDate as OrdinalDate
import Test.Bouzuya.DateTime.OrdinalDate.Component.DayOfYear as OrdinalDateComponentDayOfYear
import Test.Bouzuya.DateTime.TimeZoneOffset as TimezoneOffset
import Test.Bouzuya.DateTime.WeekDate as WeekDate
import Test.Bouzuya.DateTime.WeekDate.Component.Week as WeekDateComponentWeek
import Test.Bouzuya.DateTime.WeekDate.Component.WeekYear as WeekDateComponentWeekYear
import Test.Bouzuya.DateTime.WeekDate.Interval.WeekYear as WeekDateIntervalWeekYear
import Test.Bouzuya.DateTime.WeekDate.Interval.YearWeek as WeekDateIntervalYearWeek
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  DateIntervalYear.tests
  DateIntervalYearMonth.tests
  OffsetDateTime.tests
  OrdinalDate.tests
  OrdinalDateComponentDayOfYear.tests
  TimezoneOffset.tests
  WeekDate.tests
  WeekDateComponentWeek.tests
  WeekDateComponentWeekYear.tests
  WeekDateIntervalWeekYear.tests
  WeekDateIntervalYearWeek.tests
