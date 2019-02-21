module Test.Bouzuya.WeekDate
  ( tests
  ) where

import Bouzuya.WeekDate as WeekDate
import Prelude (bottom, discard, top)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.WeekDate" do
  test "fromDate / toDate" do
    Assert.equal bottom (WeekDate.toDate (WeekDate.fromDate bottom))
    Assert.equal top (WeekDate.toDate (WeekDate.fromDate top))
