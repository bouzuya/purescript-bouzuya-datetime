module Test.Bouzuya.WeekDate
  ( tests
  ) where

import Bouzuya.WeekDate as WeekDate
import Data.Date as Date
import Data.Enum as Enum
import Prelude (bind, bottom, discard, top, (<$>), (>>=))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.WeekDate" do
  test "fromDate / toDate" do
    let
      d1 = do
        y <- Enum.succ bottom
        m <- bottom
        d <- bottom
        Date.exactDate y m d
      d2 = do
        y <- Enum.pred top
        m <- top
        d <- top
        Date.exactDate y m d
    Assert.equal d1 (WeekDate.toDate <$> (d1 >>= WeekDate.fromDate))
    Assert.equal d2 (WeekDate.toDate <$> (d2 >>= WeekDate.fromDate))
    -- Assert.equal Nothing (WeekDate.toDate <$> (WeekDate.fromDate bottom))
    -- Assert.equal Nothing (WeekDate.toDate <$> (WeekDate.fromDate top))
