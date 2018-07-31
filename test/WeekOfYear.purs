module Test.WeekOfYear (tests) where

import Bouzuya.DateTime.Component.WeekOfYear (exactDateFromWeekOfYear, firstWeekOfYear, firstWeekdayOfYear, lastWeekOfYear, lastWeekdayOfYear, weekOfYear, weekYear)
import Data.DateTime (Weekday(..), exactDate, weekday)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
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
  test "exactDateFromWeekOfYear" do
    -- --01-04/--12-28
    Assert.equal (Just (unsafeDate 2004 1 4)) (exactDateFromWeekOfYear (unsafeYear 2004) (unsafeWeekOfYear 1) Sunday)
    Assert.equal (Just (unsafeDate 2004 1 5)) (exactDateFromWeekOfYear (unsafeYear 2004) (unsafeWeekOfYear 2) Monday)
    Assert.equal (Just (unsafeDate 2004 12 27)) (exactDateFromWeekOfYear (unsafeYear 2004) (unsafeWeekOfYear 53) Monday)
    Assert.equal (Just (unsafeDate 2004 12 28)) (exactDateFromWeekOfYear (unsafeYear 2004) (unsafeWeekOfYear 53) Tuesday)
    -- Mon -> 1 Curr
    Assert.equal (Just (unsafeDate 2000 12 31)) (exactDateFromWeekOfYear (unsafeYear 2000) (unsafeWeekOfYear 52) Sunday)
    Assert.equal (Just (unsafeDate 2001 1 1)) (exactDateFromWeekOfYear (unsafeYear 2001) (unsafeWeekOfYear 1) Monday)
    -- Tue -> 1 Curr
    Assert.equal (Just (unsafeDate 2001 12 30)) (exactDateFromWeekOfYear (unsafeYear 2001) (unsafeWeekOfYear 52) Sunday)
    Assert.equal (Just (unsafeDate 2001 12 31)) (exactDateFromWeekOfYear (unsafeYear 2002) (unsafeWeekOfYear 1) Monday)
    Assert.equal (Just (unsafeDate 2002 1 1)) (exactDateFromWeekOfYear (unsafeYear 2002) (unsafeWeekOfYear 1) Tuesday)
    -- Wed -> 1 Curr
    Assert.equal (Just (unsafeDate 2002 12 29)) (exactDateFromWeekOfYear (unsafeYear 2002) (unsafeWeekOfYear 52) Sunday)
    Assert.equal (Just (unsafeDate 2002 12 31)) (exactDateFromWeekOfYear (unsafeYear 2003) (unsafeWeekOfYear 1) Tuesday)
    Assert.equal (Just (unsafeDate 2003 1 1)) (exactDateFromWeekOfYear (unsafeYear 2003) (unsafeWeekOfYear 1) Wednesday)
    -- Thu -> 1 Curr
    Assert.equal (Just (unsafeDate 2003 12 28)) (exactDateFromWeekOfYear (unsafeYear 2003) (unsafeWeekOfYear 52) Sunday)
    Assert.equal (Just (unsafeDate 2003 12 31)) (exactDateFromWeekOfYear (unsafeYear 2004) (unsafeWeekOfYear 1) Wednesday)
    Assert.equal (Just (unsafeDate 2004 1 1)) (exactDateFromWeekOfYear (unsafeYear 2004) (unsafeWeekOfYear 1) Thursday)
    -- Fri -> 53 Prev
    Assert.equal (Just (unsafeDate 2009 12 31)) (exactDateFromWeekOfYear (unsafeYear 2009) (unsafeWeekOfYear 53) Thursday)
    Assert.equal (Just (unsafeDate 2010 1 1)) (exactDateFromWeekOfYear (unsafeYear 2009) (unsafeWeekOfYear 53) Friday)
    Assert.equal (Just (unsafeDate 2010 1 4)) (exactDateFromWeekOfYear (unsafeYear 2010) (unsafeWeekOfYear 1) Monday)
    -- Sat -> 53 Prev (prev year is leap year)
    Assert.equal (Just (unsafeDate 2004 12 31)) (exactDateFromWeekOfYear (unsafeYear 2004) (unsafeWeekOfYear 53) Friday)
    Assert.equal (Just (unsafeDate 2005 1 1)) (exactDateFromWeekOfYear (unsafeYear 2004) (unsafeWeekOfYear 53) Saturday)
    Assert.equal (Just (unsafeDate 2005 1 3)) (exactDateFromWeekOfYear (unsafeYear 2005) (unsafeWeekOfYear 1) Monday)
    -- Sat -> 52 Prev (prev year is not leap year)
    Assert.equal (Just (unsafeDate 1999 12 31)) (exactDateFromWeekOfYear (unsafeYear 1999) (unsafeWeekOfYear 52) Friday)
    Assert.equal (Just (unsafeDate 2000 1 1)) (exactDateFromWeekOfYear (unsafeYear 1999) (unsafeWeekOfYear 52) Saturday)
    Assert.equal (Just (unsafeDate 2000 1 3)) (exactDateFromWeekOfYear (unsafeYear 2000) (unsafeWeekOfYear 1) Monday)
    -- Sun -> 52 Prev
    Assert.equal (Just (unsafeDate 2005 12 31)) (exactDateFromWeekOfYear (unsafeYear 2005) (unsafeWeekOfYear 52) Saturday)
    Assert.equal (Just (unsafeDate 2006 1 1)) (exactDateFromWeekOfYear (unsafeYear 2005)  (unsafeWeekOfYear 52) Sunday)
  test "firstWeekOfYear" do
    Assert.equal (unsafeWeekOfYear 1) (firstWeekOfYear (unsafeYear 2018))
  test "firstWeekdayOfYear" do
    Assert.equal Saturday (firstWeekdayOfYear (unsafeYear 2000))
    Assert.equal Thursday (firstWeekdayOfYear (unsafeYear 2004))
    Assert.equal Thursday (firstWeekdayOfYear (unsafeYear 2015))
    Assert.equal Monday (firstWeekdayOfYear (unsafeYear 2018))
    Assert.equal Wednesday (firstWeekdayOfYear (unsafeYear 2020))
  test "lastWeekOfYear" do
    Assert.equal (unsafeWeekOfYear 52) (lastWeekOfYear (unsafeYear 2000)) -- leap year & Sat
    Assert.equal (unsafeWeekOfYear 53) (lastWeekOfYear (unsafeYear 2004)) -- leap year & *Thu*
    Assert.equal (unsafeWeekOfYear 53) (lastWeekOfYear (unsafeYear 2015)) -- not leap year & *Thu*
    Assert.equal (unsafeWeekOfYear 52) (lastWeekOfYear (unsafeYear 2018)) -- not leap year & Mon
    Assert.equal (unsafeWeekOfYear 53) (lastWeekOfYear (unsafeYear 2020)) -- *leap year* & *Wed*
  test "lastWeekdayOfYear" do
    Assert.equal (weekday (unsafeDate 2000 12 31)) (lastWeekdayOfYear (unsafeYear 2000)) -- leap year && Sat
    Assert.equal (weekday (unsafeDate 2015 12 31)) (firstWeekdayOfYear (unsafeYear 2015)) -- not leap year && Thu
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
