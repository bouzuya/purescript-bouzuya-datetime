module Test.Bouzuya.WeekDate
  ( tests
  ) where

import Bouzuya.OrdinalDate as OrdinalDate
import Bouzuya.OrdinalDate.Component.DayOfYear as DayOfYear
import Bouzuya.WeekDate (WeekDate)
import Bouzuya.WeekDate as WeekDate
import Bouzuya.WeekDate.Component.Week (Week)
import Bouzuya.WeekDate.Component.WeekYear (WeekYear)
import Bouzuya.WeekDate.Extra as WeekDateExtra
import Data.Date (Date, Weekday(..))
import Data.Date as Date
import Data.Enum as Enum
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested as TupleNested
import Prelude (bind, bottom, discard, identity, join, pure, show, top, unit, (&&), (-), (<), (<$>), (<*>), (==), (>>=))
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
            (Enum.toEnum w)
            (WeekDateExtra.firstWeekdayOfYear <$> Enum.toEnum y)
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
