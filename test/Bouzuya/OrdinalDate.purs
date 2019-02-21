module Test.Bouzuya.OrdinalDate
  ( tests
  ) where

import Bouzuya.OrdinalDate (OrdinalDate)
import Bouzuya.OrdinalDate as OrdinalDate
import Bouzuya.OrdinalDate.Component.DayOfYear as DayOfYear
import Data.Date as Date
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (bind, bottom, discard, identity, map, pure, top, unit, (&&), (<), (<$>), (<*>), (>>=))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.OrdinalDate" do
  test "Bounded OrdinalDate" do
    Assert.equal (OrdinalDate.fromDate bottom) bottom
    Assert.equal (OrdinalDate.fromDate top) top

  test "Enum OrdinalDate" do
    Assert.equal Nothing (Enum.pred bottom :: Maybe OrdinalDate)
    Assert.equal Nothing (Enum.succ top :: Maybe OrdinalDate)
    let
      y1 = Enum.toEnum 2000
      y2 = Enum.toEnum 2001
      doy1 = Enum.toEnum 1
      doy2 = Enum.toEnum 2
      doy3 = DayOfYear.lastDayOfYear <$> y1
      od1 = OrdinalDate.ordinalDate <$> y1 <*> doy1 >>= identity -- 2000-001
      od2 = OrdinalDate.ordinalDate <$> y1 <*> doy2 >>= identity -- 2000-002
      od3 = OrdinalDate.ordinalDate <$> y2 <*> doy1 >>= identity -- 2001-001
      od4 = OrdinalDate.ordinalDate <$> y2 <*> doy2 >>= identity -- 2001-002
      od5 = OrdinalDate.ordinalDate <$> y1 <*> doy3 >>= identity -- 2000-366
    Assert.equal od2 (od1 >>= Enum.succ)
    Assert.equal od4 (od3 >>= Enum.succ)
    Assert.equal od3 (od5 >>= Enum.succ)
    Assert.equal od1 (od2 >>= Enum.pred)
    Assert.equal od3 (od4 >>= Enum.pred)
    Assert.equal od5 (od3 >>= Enum.pred)

  test "Eq OrdinalDate" do -- This has been tested in other tests
    pure unit

  test "Ord OrdinalDate" do
    let
      y1 = Enum.toEnum 2000
      y2 = Enum.toEnum 2001
      doy1 = Enum.toEnum 1
      doy2 = Enum.toEnum 2
      od1 = OrdinalDate.ordinalDate <$> y1 <*> doy1 >>= identity -- 2000-001
      od2 = OrdinalDate.ordinalDate <$> y1 <*> doy2 >>= identity -- 2000-002
      od3 = OrdinalDate.ordinalDate <$> y2 <*> doy1 >>= identity -- 2001-001
      od4 = OrdinalDate.ordinalDate <$> y2 <*> doy2 >>= identity -- 2001-002
    Assert.assert "od1 < od2 < od3 < od4" (od1 < od2 && od2 < od3 && od3 < od4)

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
    let
      d20181231 = do
        y <- Enum.toEnum 2018
        m <- Enum.toEnum 12
        d <- Enum.toEnum 31
        Date.exactDate y m d
      od2018365 = do
        y <- Enum.toEnum 2018
        doy <- Enum.toEnum 365
        OrdinalDate.ordinalDate y doy
      od2018366 = do
        y <- Enum.toEnum 2018
        doy <- Enum.toEnum 366
        OrdinalDate.ordinalDate y doy
    Assert.equal d20181231 (OrdinalDate.toDate <$> od2018365)
    Assert.equal Nothing (OrdinalDate.toDate <$> od2018366)

  test "year" do
    let o1 = OrdinalDate.ordinalDate bottom bottom
    Assert.equal (Just bottom) (map OrdinalDate.year o1)
    let o2 = OrdinalDate.ordinalDate top bottom -- dummy DayOfYear
    Assert.equal (Just top) (map OrdinalDate.year o2)
