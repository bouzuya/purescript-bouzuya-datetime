module Test.OrdinalDate.Component.DayOfYear (tests) where

import Bouzuya.DateTime (Month(..), canonicalDate)
import Bouzuya.OrdinalDate.Component.DayOfYear (dayOfYear, firstDayOfYear, lastDayOfYear)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.DateTime.Component.DayOfYear" do
  let
    year2018 = (unsafePartial (fromJust (toEnum 2018)))
    year2019 = (unsafePartial (fromJust (toEnum 2019)))
    year2020 = (unsafePartial (fromJust (toEnum 2020)))
    dayOfMonth1 = (unsafePartial (fromJust (toEnum 1)))
    dayOfMonth2 = (unsafePartial (fromJust (toEnum 2)))
    dayOfYear1 = (unsafePartial (fromJust (toEnum 1)))
    dayOfYear2 = (unsafePartial (fromJust (toEnum 2)))
    dayOfYear365 = (unsafePartial (fromJust (toEnum 365)))
    dayOfYear366 = (unsafePartial (fromJust (toEnum 366)))
    date20180102 = canonicalDate year2018 January dayOfMonth2
    date20190101 = canonicalDate year2019 January dayOfMonth1
  test "dayOfYear" do
    Assert.equal (dayOfYear date20180102) dayOfYear2
  test "firstDayOfYear" do
    Assert.equal dayOfYear1 (firstDayOfYear year2018)
  test "lastDayOfYear" do
    Assert.equal dayOfYear365 (lastDayOfYear year2018)
    Assert.equal dayOfYear366 (lastDayOfYear year2020)

