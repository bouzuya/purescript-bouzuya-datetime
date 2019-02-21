module Test.Bouzuya.OrdinalDate.Component.DayOfYear
  ( tests
  ) where

import Bouzuya.OrdinalDate.Component.DayOfYear as DayOfYear
import Data.Enum as Enum
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (discard)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.OrdinalDate.Component.DayOfYear" do
  let
    year2018 = (Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum 2018)))
    year2020 = (Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum 2020)))
    dayOfYear1 = (Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum 1)))
    dayOfYear365 = (Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum 365)))
    dayOfYear366 = (Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum 366)))
  test "firstDayOfYear" do
    Assert.equal dayOfYear1 (DayOfYear.firstDayOfYear year2018)
  test "lastDayOfYear" do
    Assert.equal dayOfYear365 (DayOfYear.lastDayOfYear year2018)
    Assert.equal dayOfYear366 (DayOfYear.lastDayOfYear year2020)
