module Test.Bouzuya.WeekDate.Component.Week (tests) where

import Bouzuya.WeekDate.Component.Week (Week, firstWeekOfYear, lastWeekOfYear)
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
