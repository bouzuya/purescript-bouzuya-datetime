module Test.WeekOfYear (tests) where

import Bouzuya.DateTime.Component.WeekOfYear (firstWeekOfYear, lastWeekOfYear, startWeekdayOfYear, weekOfYear, weekYear)
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
  test "firstWeekOfYear" do
    Assert.equal (unsafeWeekOfYear 1) (firstWeekOfYear (unsafeYear 2018))
  test "lastWeekOfYear" do
    Assert.equal (unsafeWeekOfYear 52) (lastWeekOfYear (unsafeYear 2000)) -- leap year & Sat
    Assert.equal (unsafeWeekOfYear 53) (lastWeekOfYear (unsafeYear 2004)) -- leap year & *Thu*
    Assert.equal (unsafeWeekOfYear 53) (lastWeekOfYear (unsafeYear 2015)) -- not leap year & *Thu*
    Assert.equal (unsafeWeekOfYear 52) (lastWeekOfYear (unsafeYear 2018)) -- not leap year & Mon
    Assert.equal (unsafeWeekOfYear 53) (lastWeekOfYear (unsafeYear 2020)) -- *leap year* & *Wed*
  test "startWeekdayOfYear" do
    Assert.equal Saturday (startWeekdayOfYear (unsafeYear 2000))
    Assert.equal Thursday (startWeekdayOfYear (unsafeYear 2004))
    Assert.equal Thursday (startWeekdayOfYear (unsafeYear 2015))
    Assert.equal Monday (startWeekdayOfYear (unsafeYear 2018))
    Assert.equal Wednesday (startWeekdayOfYear (unsafeYear 2020))
  test "weekOfYear" do
    -- --01-04/--12-28
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2004 1 4))
    Assert.equal (unsafeWeekOfYear 2) (weekOfYear (unsafeDate 2004 1 5))
    Assert.equal (unsafeWeekOfYear 53) (weekOfYear (unsafeDate 2004 12 27))
    Assert.equal (unsafeWeekOfYear 53) (weekOfYear (unsafeDate 2004 12 28))
    -- Mon -> 1 Curr
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 2000 12 31)) -- 2000-W52-7
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2001 1 1)) -- 2001-W01-1 (Mon)
    -- Tue -> 1 Curr
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 2001 12 30)) -- 2001-W52-7
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2001 12 31)) -- 2002-W01-1
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2002 1 1)) -- 2002-W01-2 (Tue)
    -- Wed -> 1 Curr
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 2002 12 29)) -- 2002-W52-7
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2002 12 31)) -- 2003-W01-2
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2003 1 1)) -- 2003-W01-3 (Wed)
    -- Thu -> 1 Curr
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 2003 12 28)) -- 2003-W52-7
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2003 12 31)) -- 2004-W01-3
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2004 1 1)) -- 2004-W01-4 (Thu)
    -- Fri -> 53 Prev
    Assert.equal (unsafeWeekOfYear 53) (weekOfYear (unsafeDate 2009 12 31)) -- 2009-W53-4
    Assert.equal (unsafeWeekOfYear 53) (weekOfYear (unsafeDate 2010 1 1)) -- 2009-W53-5 (Fri)
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2010 1 4)) -- 2010-W01-1
    -- Sat -> 53 Prev (prev year is leap year)
    Assert.equal (unsafeWeekOfYear 53) (weekOfYear (unsafeDate 2004 12 31)) -- 2004-W53-5
    Assert.equal (unsafeWeekOfYear 53) (weekOfYear (unsafeDate 2005 1 1)) -- 2004-W53-6 (Sat)
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2005 1 3)) -- 2005-W01-1
    -- Sat -> 52 Prev (prev year is not leap year)
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 1999 12 31)) -- 1999-W52-5
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 2000 1 1)) -- 1999-W52-6 (Sat)
    Assert.equal (unsafeWeekOfYear 1) (weekOfYear (unsafeDate 2000 1 3)) -- 2000-W01-1
    -- Sun -> 52 Prev
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 2005 12 31)) -- 2005-W52-6
    Assert.equal (unsafeWeekOfYear 52) (weekOfYear (unsafeDate 2006 1 1)) -- 2005-W52-7 (Sun)
  test "weekYear" do
    -- --01-04/--12-28
    Assert.equal (unsafeYear 2004) (weekYear (unsafeDate 2004 1 4))
    Assert.equal (unsafeYear 2004) (weekYear (unsafeDate 2004 1 5))
    Assert.equal (unsafeYear 2004) (weekYear (unsafeDate 2004 12 27))
    Assert.equal (unsafeYear 2004) (weekYear (unsafeDate 2004 12 28))
    -- Mon -> 1 Curr
    Assert.equal (unsafeYear 2000) (weekYear (unsafeDate 2000 12 31)) -- 2000-W52-7
    Assert.equal (unsafeYear 2001) (weekYear (unsafeDate 2001 1 1)) -- 2001-W01-1 (Mon)
    -- Tue -> 1 Curr
    Assert.equal (unsafeYear 2001) (weekYear (unsafeDate 2001 12 30)) -- 2001-W52-7
    Assert.equal (unsafeYear 2002) (weekYear (unsafeDate 2001 12 31)) -- 2002-W01-1
    Assert.equal (unsafeYear 2002) (weekYear (unsafeDate 2002 1 1)) -- 2002-W01-2 (Tue)
    -- Wed -> 1 Curr
    Assert.equal (unsafeYear 2002) (weekYear (unsafeDate 2002 12 29)) -- 2002-W52-7
    Assert.equal (unsafeYear 2003) (weekYear (unsafeDate 2002 12 31)) -- 2003-W01-2
    Assert.equal (unsafeYear 2003) (weekYear (unsafeDate 2003 1 1)) -- 2003-W01-3 (Wed)
    -- Thu -> 1 Curr
    Assert.equal (unsafeYear 2003) (weekYear (unsafeDate 2003 12 28)) -- 2003-W52-7
    Assert.equal (unsafeYear 2004) (weekYear (unsafeDate 2003 12 31)) -- 2004-W01-3
    Assert.equal (unsafeYear 2004) (weekYear (unsafeDate 2004 1 1)) -- 2004-W01-4 (Thu)
    -- Fri -> 53 Prev
    Assert.equal (unsafeYear 2009) (weekYear (unsafeDate 2009 12 31)) -- 2009-W53-4
    Assert.equal (unsafeYear 2009) (weekYear (unsafeDate 2010 1 1)) -- 2009-W53-5 (Fri)
    Assert.equal (unsafeYear 2010) (weekYear (unsafeDate 2010 1 4)) -- 2010-W01-1
    -- Sat -> 53 Prev (prev year is leap year)
    Assert.equal (unsafeYear 2004) (weekYear (unsafeDate 2004 12 31)) -- 2004-W53-5
    Assert.equal (unsafeYear 2004) (weekYear (unsafeDate 2005 1 1)) -- 2004-W53-6 (Sat)
    Assert.equal (unsafeYear 2005) (weekYear (unsafeDate 2005 1 3)) -- 2005-W01-1
    -- Sat -> 52 Prev (prev year is not leap year)
    Assert.equal (unsafeYear 1999) (weekYear (unsafeDate 1999 12 31)) -- 1999-W52-5
    Assert.equal (unsafeYear 1999) (weekYear (unsafeDate 2000 1 1)) -- 1999-W52-6 (Sat)
    Assert.equal (unsafeYear 2000) (weekYear (unsafeDate 2000 1 3)) -- 2000-W01-1
    -- Sun -> 52 Prev
    Assert.equal (unsafeYear 2005) (weekYear (unsafeDate 2005 12 31)) -- 2005-W52-6
    Assert.equal (unsafeYear 2005) (weekYear (unsafeDate 2006 1 1)) -- 2005-W52-7 (Sun)
