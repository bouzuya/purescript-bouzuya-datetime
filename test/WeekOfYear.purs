module Test.WeekOfYear (tests) where

import Bouzuya.DateTime.Component.WeekOfYear (startWeekdayOfYear, weekOfYear, weeksInYear)
import Data.DateTime (Weekday(..), exactDate)
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (discard, join, (<$>), (<*>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.DateTime.Component.WeekOfYear" do
  let
    unsafeYear y =
      let yearMaybe = toEnum y
      in unsafePartial (fromJust yearMaybe)
    unsafeDate y m d =
      let dateMaybe = join (exactDate <$> (toEnum y) <*> (toEnum m) <*> (toEnum d))
      in unsafePartial (fromJust dateMaybe)
    unsafeWeekOfYear w =
      let weekOfYearMaybe = toEnum w
      in unsafePartial (fromJust weekOfYearMaybe)
  test "startWeekdayOfYear" do
    Assert.equal Saturday (startWeekdayOfYear (unsafeYear 2000))
    Assert.equal Thursday (startWeekdayOfYear (unsafeYear 2004))
    Assert.equal Thursday (startWeekdayOfYear (unsafeYear 2015))
    Assert.equal Monday (startWeekdayOfYear (unsafeYear 2018))
    Assert.equal Wednesday (startWeekdayOfYear (unsafeYear 2020))
  test "weekOfYear" do
    -- Thu -> 1 Curr
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2003 12 31)) -- 2004-W01-3
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2004 1 1)) -- 2004-W01-4 (Thu)
    -- Fri -> 53 Prev
    Assert.equal (unsafeWeekOfYear 53) (weekOfYear (unsafeDate 2002 12 31)) -- 2002-W53-4
    Assert.equal (unsafeWeekOfYear 53) (weekOfYear (unsafeDate 2003 1 1)) -- 2002-W53-5 (Fri)
    -- Sat -> 53 Prev (prev year is leap year)
    Assert.equal (unsafeWeekOfYear 53) (weekOfYear (unsafeDate 2004 12 31)) -- 2004-W53-5
    Assert.equal (unsafeWeekOfYear 53) (weekOfYear (unsafeDate 2005 1 1)) -- 2004-W53-6 (Sat)
    -- Sat -> 52 Prev (prev year is not leap year)
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 1999 12 31)) -- 1999-W52-5
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 2000 1 1)) -- 1999-W52-6 (Sat)
    -- Sun -> 52 Prev
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 2005 12 31)) -- 2005-W52-6
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 2006 1 1)) -- 2005-W52-7 (Sun)
  test "weeksInYear" do
    Assert.equal 52 (weeksInYear (unsafeYear 2000)) -- leap year & Sat
    Assert.equal 53 (weeksInYear (unsafeYear 2004)) -- leap year & *Thu*
    Assert.equal 53 (weeksInYear (unsafeYear 2015)) -- not leap year & *Thu*
    Assert.equal 52 (weeksInYear (unsafeYear 2018)) -- not leap year & Mon
    Assert.equal 53 (weeksInYear (unsafeYear 2020)) -- *leap year* & *Wed*
