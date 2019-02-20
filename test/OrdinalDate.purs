module Test.OrdinalDate
  ( tests
  ) where

import Bouzuya.DateTime (DayOfYear)
import Bouzuya.OrdinalDate as OrdinalDate
import Data.Date (Date, Year)
import Data.Maybe (Maybe(..))
import Prelude (bottom, discard, top)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.OrdinalDate" do
  test "fromDate / toDate" do
    let
      d1 = bottom :: Date
      d2 = top :: Date
    Assert.equal d1 (OrdinalDate.toDate (OrdinalDate.fromDate d1))
    Assert.equal d2 (OrdinalDate.toDate (OrdinalDate.fromDate d2))

  test "ordinalDate" do
    Assert.equal
      (Just (OrdinalDate.fromDate bottom))
      (OrdinalDate.ordinalDate bottom bottom)
