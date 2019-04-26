module Test.Bouzuya.DateTime.Date.Interval.Year
  ( tests
  ) where

import Bouzuya.DateTime.Date.Interval.Year as Year
import Data.Date as Date
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (bind, bottom, discard, pure, top, (<$>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.DateTime.Date.Interval.Year" do
  test "firstDate" do
    y <- pure (Enum.toEnum 2000)
    d <- pure (Year.firstDate <$> y)
    Assert.equal y (Date.year <$> d)
    Assert.equal (Just bottom) (Date.month <$> d)
    Assert.equal (Just bottom) (Date.day <$> d)

  test "lastDate" do
    y <- pure (Enum.toEnum 2000)
    d <- pure (Year.lastDate <$> y)
    Assert.equal y (Date.year <$> d)
    Assert.equal (Just top) (Date.month <$> d)
    Assert.equal (Just top) (Date.day <$> d)
