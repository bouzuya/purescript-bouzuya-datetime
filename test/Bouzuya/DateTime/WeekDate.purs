module Test.Bouzuya.DateTime.WeekDate
  ( tests
  ) where

import Bouzuya.DateTime.Date.Interval.Year as Year
import Bouzuya.DateTime.OrdinalDate as OrdinalDate
import Bouzuya.DateTime.WeekDate (Week, WeekDate, WeekYear)
import Bouzuya.DateTime.WeekDate as WeekDate
import Bouzuya.DateTime.WeekDate.Interval.WeekYear as WeekYear
import Data.Date (Date, Day, Month, Weekday(..), Year)
import Data.Date as Date
import Data.Enum as Enum
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested as TupleNested
import Prelude (bind, negate, bottom, discard, identity, join, pure, show, top, unit, (&&), (-), (<), (<$>), (<*>), (<<<), (==), (>>=))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.DateTime.WeekDate" do
  test "bottom :: Date" do
    -- -271820-01-01
    let date = bottom :: Date
    Assert.equal (bottom :: Year) (Date.year date)
    Assert.equal ((Enum.toEnum (-271820)) :: _ Year) (Just (bottom :: Year))
    Assert.equal true (Date.isLeapYear (bottom :: Year))
    Assert.equal (bottom :: Month) (Date.month date)
    Assert.equal ((Enum.toEnum 1) :: _ Month) (Just (bottom :: Month))
    Assert.equal (bottom :: Day) (Date.day date)
    Assert.equal ((Enum.toEnum 1) :: _ Day) (Just (bottom :: Day))
    Assert.equal Date.Saturday (Date.weekday date)

  test "top :: Date" do
    -- 275759-12-31
    let date = top :: Date
    Assert.equal (top :: Year) (Date.year date)
    Assert.equal ((Enum.toEnum 275759) :: _ Year) (Just (top :: Year))
    Assert.equal false (Date.isLeapYear (top :: Year))
    Assert.equal (top :: Month) (Date.month date)
    Assert.equal ((Enum.toEnum 12) :: _ Month) (Just (top :: Month))
    Assert.equal (top :: Day) (Date.day date)
    Assert.equal ((Enum.toEnum 31) :: _ Day) (Just (top :: Day))
    Assert.equal Date.Monday (Date.weekday date)

  test "Bounded WeekDate" do
    Assert.equal (WeekDate.fromDate (bottom :: Date)) (bottom :: WeekDate)
    Assert.equal (WeekDate.fromDate (top :: Date)) (top :: WeekDate)

  test "Enum WeekDate" do
    Assert.equal Nothing (Enum.pred bottom :: Maybe WeekDate)
    Assert.equal Nothing (Enum.succ top :: Maybe WeekDate)
    let
      y1 = Enum.toEnum 2000
      y2 = Enum.toEnum 2001
      doy1 = Enum.toEnum 1
      doy2 = Enum.toEnum 2
      doy3 =
        (OrdinalDate.dayOfYear <<< OrdinalDate.lastOrdinalDateOfYear) <$> y1
      wd1 = do
        od <- OrdinalDate.ordinalDate <$> y1 <*> doy1 >>= identity
        d <- pure (OrdinalDate.toDate od)
        pure (WeekDate.fromDate d)  -- 2000-001
      wd2 = do
        od <- OrdinalDate.ordinalDate <$> y1 <*> doy2 >>= identity
        d <- pure (OrdinalDate.toDate od)
        pure (WeekDate.fromDate d)  -- 2000-002
      wd3 = do
        od <- OrdinalDate.ordinalDate <$> y2 <*> doy1 >>= identity
        d <- pure (OrdinalDate.toDate od)
        pure (WeekDate.fromDate d)  -- 2001-001
      wd4 = do
        od <- OrdinalDate.ordinalDate <$> y2 <*> doy2 >>= identity
        d <- pure (OrdinalDate.toDate od)
        pure (WeekDate.fromDate d)  -- 2001-002
      wd5 = do
        od <- OrdinalDate.ordinalDate <$> y1 <*> doy3 >>= identity
        d <- pure (OrdinalDate.toDate od)
        pure (WeekDate.fromDate d)  -- 2000-366
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
        pure (WeekDate.fromDate d)  -- 2000-001
      wd2 = do
        od <- OrdinalDate.ordinalDate <$> y1 <*> doy2 >>= identity
        d <- pure (OrdinalDate.toDate od)
        pure (WeekDate.fromDate d)  -- 2000-002
      wd3 = do
        od <- OrdinalDate.ordinalDate <$> y2 <*> doy1 >>= identity
        d <- pure (OrdinalDate.toDate od)
        pure (WeekDate.fromDate d)  -- 2001-001
      wd4 = do
        od <- OrdinalDate.ordinalDate <$> y2 <*> doy2 >>= identity
        d <- pure (OrdinalDate.toDate od)
        pure (WeekDate.fromDate d)  -- 2001-002
    Assert.assert "wd1 < wd2 < wd3 < wd4" (wd1 < wd2 && wd2 < wd3 && wd3 < wd4)

  test "Show WeekDate" do
    Assert.equal
      "(WeekDate (WeekYear -271821) (Week 52) Saturday)"
      (show (WeekDate.fromDate bottom))
    Assert.equal
      "(WeekDate (WeekYear 275760) (Week 1) Monday)"
      (show (WeekDate.fromDate top))

  test "firstWeekDateOfWeek" do
    let
      wy1 = bottom
      w1 = top
    Assert.equal Nothing (WeekDate.firstWeekDateOfWeek wy1 w1)
    let
      wy2 = Enum.succ bottom
      w2 = bottom
    Assert.equal
      (Just "(WeekDate (WeekYear -271820) (Week 1) Monday)")
      (show <$> (wy2 >>= \wy -> WeekDate.firstWeekDateOfWeek wy w2))
    let
      wy3 = top
      w3 = bottom
    Assert.equal
      (Just "(WeekDate (WeekYear 275760) (Week 1) Monday)")
      (show <$> (wy3 >>= \wy -> WeekDate.firstWeekDateOfWeek wy w3))

  test "fromDate / toDate" do
    Assert.equal bottom (WeekDate.toDate (WeekDate.fromDate bottom))
    Assert.equal top (WeekDate.toDate (WeekDate.fromDate top))

  test "lastWeekDateOfWeek" do
    let
      wy1 = bottom :: WeekYear
      w1 = WeekDate.week <$> (WeekYear.lastWeekDate wy1)
    Assert.equal
      (Just "(WeekDate (WeekYear -271821) (Week 52) Sunday)")
      (show <$> (w1 >>= (WeekDate.lastWeekDateOfWeek wy1)))
    let
      wy2 = Enum.pred (top :: WeekYear)
      w2 = WeekDate.week <$> (wy2 >>= WeekYear.lastWeekDate)
    Assert.equal
      (Just "(WeekDate (WeekYear 275759) (Week 52) Sunday)")
      (show <$> (WeekDate.lastWeekDateOfWeek <$> wy2 <*> w2 >>= identity))
    let
      wy3 = top
      w3 = bottom
    Assert.equal
      Nothing
      (show <$> (WeekDate.lastWeekDateOfWeek wy3 w3))

  test "weekDate (firstWeekdayOfYear)" do
    let
      exactDateFromWeek :: WeekYear -> Week -> Weekday -> Maybe Date
      exactDateFromWeek wy w wday =
        WeekDate.toDate <$> (WeekDate.weekDate wy w wday)
    Foldable.for_
      [ TupleNested.tuple6 "Mon YYYY-W01-1 = YYYY-01-01" 1 2018 0 1 1
      , TupleNested.tuple6 "Tue YYYY-W01-1 = YYYY-12-31" 2 2019 1 12 31
      , TupleNested.tuple6 "Wed YYYY-W01-1 = YYYY-12-30" 3 2020 1 12 30
      , TupleNested.tuple6 "Thu YYYY-W01-1 = YYYY-12-29" 4 2026 1 12 29
      , TupleNested.tuple6 "Fri YYYY-W01-1 = YYYY-01-04" 5 2021 0 1 4
      , TupleNested.tuple6 "Sat YYYY-W01-1 = YYYY-01-03" 6 2022 0 1 3
      , TupleNested.tuple6 "Sun YYYY-W01-1 = YYYY-01-02" 7 2023 0 1 2
      ]
      (TupleNested.uncurry6
        (\s w y o m d -> do
          Assert.equal
            ((Enum.toEnum w) :: _ Weekday)
            ((Date.weekday <<< Year.firstDate) <$> Enum.toEnum y)
          Assert.equal'
            s
            (join (Date.exactDate <$> Enum.toEnum (y - o) <*> Enum.toEnum m <*> Enum.toEnum d))
            (Enum.toEnum y >>= \y' -> exactDateFromWeek y' bottom bottom)))
  test "weekDate" do
    Foldable.for_
      [ TupleNested.tuple7 "--01-04/--12-28 (1)" 2004 1 4 2004 1 Sunday
      , TupleNested.tuple7 "--01-04/--12-28 (2)" 2004 1 5 2004 2 Monday
      , TupleNested.tuple7 "--01-04/--12-28 (3)" 2004 12 27 2004 53 Monday
      , TupleNested.tuple7 "--01-04/--12-28 (4)" 2004 12 28 2004 53 Tuesday
      , TupleNested.tuple7 "Mon -> 1 Curr (1)" 2000 12 31 2000 52 Sunday
      , TupleNested.tuple7 "Mon -> 1 Curr (2)" 2001 1 1 2001 1 Monday
      , TupleNested.tuple7 "Tue -> 1 Curr (1)" 2001 12 30 2001 52 Sunday
      , TupleNested.tuple7 "Tue -> 1 Curr (2)" 2001 12 31 2002 1 Monday
      , TupleNested.tuple7 "Tue -> 1 Curr (3)" 2002 1 1 2002 1 Tuesday
      , TupleNested.tuple7 "Wed -> 1 Curr (1)" 2002 12 29 2002 52 Sunday
      , TupleNested.tuple7 "Wed -> 1 Curr (2)" 2002 12 31 2003 1 Tuesday
      , TupleNested.tuple7 "Wed -> 1 Curr (3)" 2003 1 1 2003 1 Wednesday
      , TupleNested.tuple7 "Thu -> 1 Curr (1)" 2003 12 28 2003 52 Sunday
      , TupleNested.tuple7 "Thu -> 1 Curr (2)" 2003 12 31 2004 1 Wednesday
      , TupleNested.tuple7 "Thu -> 1 Curr (3)" 2004 1 1 2004 1 Thursday
      , TupleNested.tuple7 "Fri -> 53 Prev (1)" 2009 12 31 2009 53 Thursday
      , TupleNested.tuple7 "Fri -> 53 Prev (2)" 2010 1 1 2009 53 Friday
      , TupleNested.tuple7 "Fri -> 53 Prev (3)" 2010 1 4 2010 1 Monday
      , TupleNested.tuple7 "Sat -> 53 Prev (1)" 2004 12 31 2004 53 Friday
      , TupleNested.tuple7 "Sat -> 53 Prev (2)" 2005 1 1 2004 53 Saturday
      , TupleNested.tuple7 "Sat -> 53 Prev (3)" 2005 1 3 2005 1 Monday
      , TupleNested.tuple7 "Sat -> 52 Prev (1)" 1999 12 31 1999 52 Friday
      , TupleNested.tuple7 "Sat -> 52 Prev (2)" 2000 1 1 1999 52 Saturday
      , TupleNested.tuple7 "Sat -> 52 Prev (3)" 2000 1 3 2000 1 Monday
      , TupleNested.tuple7 "Sun -> 52 Prev (1)" 2005 12 31 2005 52 Saturday
      , TupleNested.tuple7 "Sun -> 52 Prev (2)" 2006 1 1 2005 52 Sunday
      , TupleNested.tuple7 "Others (1)" 2019 2 22 2019 8 Friday
      , TupleNested.tuple7 "Others (2)" 2020 7 24 2020 30 Friday
      ]
      (TupleNested.uncurry7
        (\s y m d wy w wday -> do
          let
            date :: Maybe Date
            date = do
              y' <- Enum.toEnum y
              m' <- Enum.toEnum m
              d' <- Enum.toEnum d
              Date.exactDate y' m' d'
            weekDate :: Maybe Date
            weekDate = do
              wy' <- Enum.toEnum wy
              w' <- Enum.toEnum w
              WeekDate.toDate <$> (WeekDate.weekDate wy' w' wday)
          Assert.assert s (date == weekDate)
          Assert.equal date weekDate))
