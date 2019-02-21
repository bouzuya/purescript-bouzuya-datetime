module Test.Bouzuya.WeekDate
  ( tests
  ) where

import Bouzuya.OrdinalDate as OrdinalDate
import Bouzuya.OrdinalDate.Component.DayOfYear as DayOfYear
import Bouzuya.WeekDate (WeekDate)
import Bouzuya.WeekDate as WeekDate
import Data.Date as Date
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (bind, bottom, discard, identity, pure, show, top, unit, (&&), (<), (<$>), (<*>), (>>=))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.WeekDate" do
  test "Bounded WeekDate" do
    let
      d1 = do
        y <- Enum.succ bottom
        Date.exactDate y bottom bottom
      d2 = do
        y <- Enum.pred top
        Date.exactDate y top top
    Assert.equal (d1 >>= WeekDate.fromDate) (Just (bottom :: WeekDate))
    Assert.equal (d2 >>= WeekDate.fromDate) (Just (top :: WeekDate))

  test "Enum WeekDate" do
    Assert.equal Nothing (Enum.pred bottom :: Maybe WeekDate)
    Assert.equal Nothing (Enum.succ top :: Maybe WeekDate)
    let
      y1 = Enum.toEnum 2000
      y2 = Enum.toEnum 2001
      doy1 = Enum.toEnum 1
      doy2 = Enum.toEnum 2
      doy3 = DayOfYear.lastDayOfYear <$> y1
      wd1 = do
        od <- OrdinalDate.ordinalDate <$> y1 <*> doy1 >>= identity
        d <- pure (OrdinalDate.toDate od)
        WeekDate.fromDate d  -- 2000-001
      wd2 = do
        od <- OrdinalDate.ordinalDate <$> y1 <*> doy2 >>= identity
        d <- pure (OrdinalDate.toDate od)
        WeekDate.fromDate d  -- 2000-002
      wd3 = do
        od <- OrdinalDate.ordinalDate <$> y2 <*> doy1 >>= identity
        d <- pure (OrdinalDate.toDate od)
        WeekDate.fromDate d  -- 2001-001
      wd4 = do
        od <- OrdinalDate.ordinalDate <$> y2 <*> doy2 >>= identity
        d <- pure (OrdinalDate.toDate od)
        WeekDate.fromDate d  -- 2001-002
      wd5 = do
        od <- OrdinalDate.ordinalDate <$> y1 <*> doy3 >>= identity
        d <- pure (OrdinalDate.toDate od)
        WeekDate.fromDate d  -- 2000-366
    Assert.equal wd2 (wd1 >>= Enum.succ)
    Assert.equal wd4 (wd3 >>= Enum.succ)
    Assert.equal wd3 (wd5 >>= Enum.succ)
    Assert.equal wd1 (wd2 >>= Enum.pred)
    Assert.equal wd3 (wd4 >>= Enum.pred)
    Assert.equal wd5 (wd3 >>= Enum.pred)

  test "Eq WeekDate" do -- This has been tested in other tests
    pure unit

  test "Ord WeekDate" do
    let
      y1 = Enum.toEnum 2000
      y2 = Enum.toEnum 2001
      doy1 = Enum.toEnum 1
      doy2 = Enum.toEnum 2
      wd1 = do
        od <- OrdinalDate.ordinalDate <$> y1 <*> doy1 >>= identity
        d <- pure (OrdinalDate.toDate od)
        WeekDate.fromDate d  -- 2000-001
      wd2 = do
        od <- OrdinalDate.ordinalDate <$> y1 <*> doy2 >>= identity
        d <- pure (OrdinalDate.toDate od)
        WeekDate.fromDate d  -- 2000-002
      wd3 = do
        od <- OrdinalDate.ordinalDate <$> y2 <*> doy1 >>= identity
        d <- pure (OrdinalDate.toDate od)
        WeekDate.fromDate d  -- 2001-001
      wd4 = do
        od <- OrdinalDate.ordinalDate <$> y2 <*> doy2 >>= identity
        d <- pure (OrdinalDate.toDate od)
        WeekDate.fromDate d  -- 2001-002
    Assert.assert "wd1 < wd2 < wd3 < wd4" (wd1 < wd2 && wd2 < wd3 && wd3 < wd4)

  test "Show WeekDate" do
    let
      wd = do
        y <- Enum.succ bottom
        d <- Date.exactDate y bottom bottom
        WeekDate.fromDate d
    Assert.equal
      (Just "(WeekDate (WeekYear -271819) (Week 1) Monday)")
      (show <$> wd)

  test "fromDate / toDate" do
    let
      d1 = do
        y <- Enum.succ bottom
        Date.exactDate y bottom bottom
      d2 = do
        y <- Enum.pred top
        Date.exactDate y top top
    Assert.equal d1 (WeekDate.toDate <$> (d1 >>= WeekDate.fromDate))
    Assert.equal d2 (WeekDate.toDate <$> (d2 >>= WeekDate.fromDate))
    Assert.equal Nothing (WeekDate.toDate <$> (WeekDate.fromDate bottom))
    Assert.equal Nothing (WeekDate.toDate <$> (WeekDate.fromDate top))
