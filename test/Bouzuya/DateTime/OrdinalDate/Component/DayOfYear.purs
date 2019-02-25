module Test.Bouzuya.DateTime.OrdinalDate.Component.DayOfYear
  ( tests
  ) where

import Bouzuya.DateTime.OrdinalDate (DayOfYear)
import Bouzuya.DateTime.OrdinalDate.Component.DayOfYear as DayOfYear
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (bottom, discard, pure, show, top, unit, (<))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.DateTime.OrdinalDate.Component.DayOfYear" do
  test "Bounded DayOfYear" do
    Assert.equal ((Enum.toEnum 1) :: _ DayOfYear) (Just (bottom :: DayOfYear))
    Assert.equal ((Enum.toEnum 366) :: _ DayOfYear) (Just (top :: DayOfYear))

  test "BoundedEnum DayOfYear" do
    let (Enum.Cardinality n) = Enum.cardinality :: _ DayOfYear
    Assert.equal 1 (Enum.fromEnum (bottom :: DayOfYear))
    Assert.equal 366 (Enum.fromEnum (top :: DayOfYear))
    Assert.equal Nothing ((Enum.toEnum 0) :: _ DayOfYear)
    Assert.equal (Just (bottom :: DayOfYear)) ((Enum.toEnum 1) :: _ DayOfYear)
    Assert.equal (Just (top :: DayOfYear)) ((Enum.toEnum 366) :: _ DayOfYear)
    Assert.equal Nothing ((Enum.toEnum 367) :: _ DayOfYear)

  test "Enum DayOfYear" do
    Assert.equal Nothing (Enum.pred (bottom :: DayOfYear))
    Assert.equal Nothing (Enum.succ (top :: DayOfYear))
    Assert.equal (Enum.toEnum 365) (Enum.pred (top :: DayOfYear))
    Assert.equal (Enum.toEnum 2) (Enum.succ (bottom :: DayOfYear))

  test "Eq DayOfYear" do
    pure unit

  test "Ord DayOfYear" do
    Assert.assert
      "(DayOfYear 1) < (DayOfYear 366)"
      ((bottom :: DayOfYear) < (top :: DayOfYear))

  test "Show DayOfYear" do
    Assert.equal "(DayOfYear 1)" (show (bottom :: DayOfYear))
    Assert.equal "(DayOfYear 366)" (show (top :: DayOfYear))

  test "lastDayOfYearOfLongYear" do
    Assert.equal "(DayOfYear 366)" (show (DayOfYear.lastDayOfYearOfLongYear))

  test "lastDayOfYearOfShortYear" do
    Assert.equal "(DayOfYear 365)" (show (DayOfYear.lastDayOfYearOfShortYear))
