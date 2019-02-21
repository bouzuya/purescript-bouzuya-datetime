module Test.OrdinalDate.Component.DayOfYear (tests) where

import Bouzuya.OrdinalDate.Component.DayOfYear (firstDayOfYear, lastDayOfYear)
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.DateTime.Component.DayOfYear" do
  let
    year2018 = (unsafePartial (fromJust (toEnum 2018)))
    year2020 = (unsafePartial (fromJust (toEnum 2020)))
    dayOfYear1 = (unsafePartial (fromJust (toEnum 1)))
    dayOfYear365 = (unsafePartial (fromJust (toEnum 365)))
    dayOfYear366 = (unsafePartial (fromJust (toEnum 366)))
  test "firstDayOfYear" do
    Assert.equal dayOfYear1 (firstDayOfYear year2018)
  test "lastDayOfYear" do
    Assert.equal dayOfYear365 (lastDayOfYear year2018)
    Assert.equal dayOfYear366 (lastDayOfYear year2020)
