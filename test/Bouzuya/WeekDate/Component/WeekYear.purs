module Test.Bouzuya.WeekDate.Component.WeekYear
  ( tests
  ) where

import Bouzuya.WeekDate.Component.WeekYear (WeekYear)
import Data.Date (Year)
import Data.Enum (Cardinality(..))
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Data.Ord as Ord
import Prelude (bottom, discard, negate, pure, show, top, unit, (+), (<), (<$>), (>>=))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.WeekDate.Component.WeekYear" do
  test "Bounded WeekYear" do
    Assert.equal
      (Just (bottom :: WeekYear))
      (Enum.toEnum ((Enum.fromEnum (bottom :: Year)) + 1))
    Assert.equal
      (Just (top :: WeekYear))
      (Enum.toEnum (Enum.fromEnum (top :: Year)))

  test "BoundedEnum WeekYear" do
    Assert.equal
      (Enum.cardinality :: Cardinality WeekYear)
      (Cardinality
        ((Ord.abs (Enum.fromEnum (bottom :: WeekYear))) +
          (Enum.fromEnum (top :: WeekYear)) + 1))
    Assert.equal
      (Just (bottom :: WeekYear))
      (Enum.toEnum (Enum.fromEnum (bottom :: WeekYear)))
    Assert.equal
      (Just (top :: WeekYear))
      (Enum.toEnum (Enum.fromEnum (top :: WeekYear)))
    Assert.equal
      (Nothing :: Maybe WeekYear)
      (Enum.toEnum ((Enum.fromEnum (top :: WeekYear)) + 1))

  test "Enum WeekYear" do
    Assert.equal Nothing (Enum.pred bottom :: Maybe WeekYear)
    Assert.equal Nothing (Enum.succ top :: Maybe WeekYear)
    Assert.equal
      ((Enum.toEnum 2001) :: Maybe WeekYear)
      ((Enum.toEnum 2000) >>= Enum.succ)
    Assert.equal
      ((Enum.toEnum 2000) :: Maybe WeekYear)
      ((Enum.toEnum 2001) >>= Enum.pred)

  test "Eq WeekYear" do -- This has been tested in other tests
    pure unit

  test "Ord WeekYear" do
    Assert.assert
      "WeekYear 2000 < WeekYear 2001"
      (((Enum.toEnum 2000) :: Maybe WeekYear) <
        ((Enum.toEnum 2001) :: Maybe WeekYear))

  test "Show WeekYear" do
    Assert.equal
      (Just "(WeekYear 2000)")
      (show <$> ((Enum.toEnum 2000) :: _ WeekYear))
