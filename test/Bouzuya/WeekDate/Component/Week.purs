module Test.Bouzuya.WeekDate.Component.Week (tests) where

import Bouzuya.WeekDate.Component.Week (Week, firstWeekOfYear, lastWeekOfYear, week, weekYear)
import Data.Date (Year)
import Data.Date as Date
import Data.Enum as Enum
import Data.Foldable as Foldable
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Partial.Unsafe as Unsafe
import Prelude (bind, discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.WeekDate.Component.Week" do
  let
    unsafeYear y =
      let yearMaybe = Enum.toEnum y :: _ Year
      in Unsafe.unsafePartial (Maybe.fromJust yearMaybe)
    unsafeDate y m d =
      let
        dateMaybe = do
          y' <- Enum.toEnum y
          m' <- Enum.toEnum m
          d' <- Enum.toEnum d
          Date.exactDate y' m' d'
      in Unsafe.unsafePartial (Maybe.fromJust dateMaybe)
    unsafeWeek w =
      let weekMaybe = Enum.toEnum w :: _ Week
      in Unsafe.unsafePartial (Maybe.fromJust weekMaybe)
  test "firstWeekOfYear" do
    Assert.equal (unsafeWeek 1) (firstWeekOfYear (unsafeYear 2018))
  test "lastWeekOfYear" do
    Foldable.for_
      [ Tuple 52 2000 -- leap year & Sat
      , Tuple 53 2004 -- leap year & *Thu*
      , Tuple 53 2015 -- not leap year & *Thu*
      , Tuple 52 2018 -- not leap year & Mon
      , Tuple 53 2020 -- *leap year* & *Wed*
      ]
      \(Tuple w y) -> Assert.equal (unsafeWeek w) (lastWeekOfYear (unsafeYear y))
  test "week" do
    -- --01-04/--12-28
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2004 1 4))
    Assert.equal (unsafeWeek 2) (week (unsafeDate 2004 1 5))
    Assert.equal (unsafeWeek 53) (week (unsafeDate 2004 12 27))
    Assert.equal (unsafeWeek 53) (week (unsafeDate 2004 12 28))
    -- Mon -> 1 Curr
    Assert.equal (unsafeWeek 52) (week (unsafeDate 2000 12 31)) -- 2000-W52-7
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2001 1 1)) -- 2001-W01-1 (Mon)
    -- Tue -> 1 Curr
    Assert.equal (unsafeWeek 52) (week (unsafeDate 2001 12 30)) -- 2001-W52-7
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2001 12 31)) -- 2002-W01-1
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2002 1 1)) -- 2002-W01-2 (Tue)
    -- Wed -> 1 Curr
    Assert.equal (unsafeWeek 52) (week (unsafeDate 2002 12 29)) -- 2002-W52-7
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2002 12 31)) -- 2003-W01-2
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2003 1 1)) -- 2003-W01-3 (Wed)
    -- Thu -> 1 Curr
    Assert.equal (unsafeWeek 52) (week (unsafeDate 2003 12 28)) -- 2003-W52-7
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2003 12 31)) -- 2004-W01-3
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2004 1 1)) -- 2004-W01-4 (Thu)
    -- Fri -> 53 Prev
    Assert.equal (unsafeWeek 53) (week (unsafeDate 2009 12 31)) -- 2009-W53-4
    Assert.equal (unsafeWeek 53) (week (unsafeDate 2010 1 1)) -- 2009-W53-5 (Fri)
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2010 1 4)) -- 2010-W01-1
    -- Sat -> 53 Prev (prev year is leap year)
    Assert.equal (unsafeWeek 53) (week (unsafeDate 2004 12 31)) -- 2004-W53-5
    Assert.equal (unsafeWeek 53) (week (unsafeDate 2005 1 1)) -- 2004-W53-6 (Sat)
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2005 1 3)) -- 2005-W01-1
    -- Sat -> 52 Prev (prev year is not leap year)
    Assert.equal (unsafeWeek 52) (week (unsafeDate 1999 12 31)) -- 1999-W52-5
    Assert.equal (unsafeWeek 52) (week (unsafeDate 2000 1 1)) -- 1999-W52-6 (Sat)
    Assert.equal (unsafeWeek 1) (week (unsafeDate 2000 1 3)) -- 2000-W01-1
    -- Sun -> 52 Prev
    Assert.equal (unsafeWeek 52) (week (unsafeDate 2005 12 31)) -- 2005-W52-6
    Assert.equal (unsafeWeek 52) (week (unsafeDate 2006 1 1)) -- 2005-W52-7 (Sun)
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
