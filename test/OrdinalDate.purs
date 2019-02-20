module Test.OrdinalDate
  ( tests
  ) where

import Bouzuya.OrdinalDate as OrdinalDate
import Data.Maybe (Maybe(..))
import Prelude (bottom, discard, map, top)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.OrdinalDate" do
  test "dayOfYear" do
    let o1 = OrdinalDate.ordinalDate bottom bottom
    Assert.equal (Just bottom) (map OrdinalDate.dayOfYear o1)
    let o2 = OrdinalDate.ordinalDate bottom top -- dummy year
    Assert.equal (Just top) (map OrdinalDate.dayOfYear o2)

  test "fromDate / toDate" do
    Assert.equal bottom (OrdinalDate.toDate (OrdinalDate.fromDate bottom))
    Assert.equal top (OrdinalDate.toDate (OrdinalDate.fromDate top))

  test "ordinalDate" do
    Assert.equal
      (Just (OrdinalDate.fromDate bottom))
      (OrdinalDate.ordinalDate bottom bottom)
    -- top :: DayOfYear is out of range ?

  test "year" do
    let o1 = OrdinalDate.ordinalDate bottom bottom
    Assert.equal (Just bottom) (map OrdinalDate.year o1)
    let o2 = OrdinalDate.ordinalDate top bottom -- dummy DayOfYear
    Assert.equal (Just top) (map OrdinalDate.year o2)
