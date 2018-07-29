module Test.WeekOfYear (tests) where

import Data.DateTime (Weekday(..))
import Bouzuya.DateTime.Component.WeekOfYear (startWeekdayOfYear, weeksInYear)
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.DateTime.Component.WeekOfYear" do
  let
    unsafeYear n = unsafePartial (fromJust (toEnum n))
  test "startWeekdayOfYear" do
    Assert.equal Saturday (startWeekdayOfYear (unsafeYear 2000))
    Assert.equal Thursday (startWeekdayOfYear (unsafeYear 2004))
    Assert.equal Thursday (startWeekdayOfYear (unsafeYear 2015))
    Assert.equal Monday (startWeekdayOfYear (unsafeYear 2018))
    Assert.equal Wednesday (startWeekdayOfYear (unsafeYear 2020))
  test "weeksInYear" do
    Assert.equal 52 (weeksInYear (unsafeYear 2000)) -- leap year & Sat
    Assert.equal 53 (weeksInYear (unsafeYear 2004)) -- leap year & *Thu*
    Assert.equal 53 (weeksInYear (unsafeYear 2015)) -- not leap year & *Thu*
    Assert.equal 52 (weeksInYear (unsafeYear 2018)) -- not leap year & Mon
    Assert.equal 53 (weeksInYear (unsafeYear 2020)) -- *leap year* & *Wed*
