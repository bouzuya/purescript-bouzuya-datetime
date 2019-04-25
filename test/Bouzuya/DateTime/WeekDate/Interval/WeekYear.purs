module Test.Bouzuya.DateTime.WeekDate.Interval.WeekYear
  ( tests
  ) where

import Prelude

import Bouzuya.DateTime.WeekDate (WeekYear)
import Bouzuya.DateTime.WeekDate.Interval.WeekYear as WeekYear
import Data.Enum as Enum
import Data.Maybe as Maybe
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Bouzuya.DateTime.WeekDate.Interval.WeekYear" do

  TestUnit.test "firstWeekDate" do
    let wy1 = bottom :: WeekYear
    Assert.equal Maybe.Nothing (WeekYear.firstWeekDate wy1)
    let wy2 = Enum.succ (bottom :: WeekYear)
    Assert.equal
      (Maybe.Just "(WeekDate (WeekYear -271820) (Week 1) Monday)")
      (show <$> (wy2 >>= \wy -> WeekYear.firstWeekDate wy))
    let wy3 = top
    Assert.equal
      (Maybe.Just "(WeekDate (WeekYear 275760) (Week 1) Monday)")
      (show <$> (WeekYear.firstWeekDate wy3))
