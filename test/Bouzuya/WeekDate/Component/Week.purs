module Test.Bouzuya.WeekDate.Component.Week (tests) where

import Bouzuya.WeekDate (Week)
import Bouzuya.WeekDate.Component.Week as Week
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (bottom, discard, pure, show, top, unit, (<))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.WeekDate.Component.Week" do
  test "Bounded Week" do
    Assert.equal ((Enum.toEnum 1) :: _ Week) (Just (bottom :: Week))
    Assert.equal ((Enum.toEnum 53) :: _ Week) (Just (top :: Week))

  test "BoundedEnum Week" do
    let (Enum.Cardinality n) = Enum.cardinality :: _ Week
    Assert.equal 1 (Enum.fromEnum (bottom :: Week))
    Assert.equal 53 (Enum.fromEnum (top :: Week))
    Assert.equal Nothing ((Enum.toEnum 0) :: _ Week)
    Assert.equal (Just (bottom :: Week)) ((Enum.toEnum 1) :: _ Week)
    Assert.equal (Just (top :: Week)) ((Enum.toEnum 53) :: _ Week)
    Assert.equal Nothing ((Enum.toEnum 54) :: _ Week)

  test "Enum Week" do
    Assert.equal Nothing (Enum.pred (bottom :: Week))
    Assert.equal Nothing (Enum.succ (top :: Week))
    Assert.equal (Enum.toEnum 52) (Enum.pred (top :: Week))
    Assert.equal (Enum.toEnum 2) (Enum.succ (bottom :: Week))

  test "Eq Week" do
    pure unit

  test "Ord Week" do
    Assert.assert
      "(Week 1) < (Week 53)"
      ((bottom :: Week) < (top :: Week))

  test "Show Week" do
    Assert.equal "(Week 1)" (show (bottom :: Week))
    Assert.equal "(Week 53)" (show (top :: Week))

  test "lastWeekOfLongYear" do
    Assert.equal "(Week 53)" (show (Week.lastWeekOfLongYear))

  test "lastWeekOfShortYear" do
    Assert.equal "(Week 52)" (show (Week.lastWeekOfShortYear))
