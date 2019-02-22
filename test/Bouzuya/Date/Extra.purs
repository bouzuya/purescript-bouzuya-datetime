module Test.Bouzuya.Date.Extra
  ( tests
  ) where

import Bouzuya.Date.Extra as DateExtra
import Data.Date as Date
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (bind, bottom, discard, pure, top, (<$>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.Date.Extra" do
  test "firstDateOfYear" do
    y <- pure (Enum.toEnum 2000)
    d <- pure (DateExtra.firstDateOfYear <$> y)
    Assert.equal y (Date.year <$> d)
    Assert.equal (Just bottom) (Date.month <$> d)
    Assert.equal (Just bottom) (Date.day <$> d)

  test "lastDateOfYear" do
    y <- pure (Enum.toEnum 2000)
    d <- pure (DateExtra.lastDateOfYear <$> y)
    Assert.equal y (Date.year <$> d)
    Assert.equal (Just top) (Date.month <$> d)
    Assert.equal (Just top) (Date.day <$> d)
