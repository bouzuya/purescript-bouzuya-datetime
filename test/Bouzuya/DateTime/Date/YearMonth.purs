module Test.Bouzuya.DateTime.Date.YearMonth
  ( tests
  ) where

import Bouzuya.DateTime.Date.YearMonth (YearMonth)
import Bouzuya.DateTime.Date.YearMonth as YearMonth
import Data.Date (Day, Year)
import Data.Date as Date
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (bind, bottom, discard, pure, show, top, unit, (*), (<), (<$>), (<*>), (<=), (>>=))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.DateTime.Date.YearMonth" do
  test "Bounded YearMonth" do
    Assert.equal (YearMonth.fromDate bottom) (bottom :: YearMonth)
    Assert.equal (YearMonth.fromDate top) (top :: YearMonth)
    Assert.assert "bottom <= top" ((bottom :: YearMonth) <= (top :: YearMonth))

  test "BoundedEnum YearMonth" do
    let
      (Enum.Cardinality yn) = Enum.cardinality :: _ Year
      (Enum.Cardinality ymn) = Enum.cardinality :: _ YearMonth
    Assert.equal (yn * 12) ymn
    Assert.equal
      (Just (bottom :: YearMonth))
      (Enum.toEnum (Enum.fromEnum (bottom :: YearMonth)))
    Assert.equal
      (Just (top :: YearMonth))
      (Enum.toEnum (Enum.fromEnum (top :: YearMonth)))

  test "Enum YearMonth" do
    Assert.equal Nothing ((Enum.pred bottom) :: _ YearMonth)
    Assert.equal Nothing ((Enum.succ top) :: _ YearMonth)
    Assert.equal
      ((Enum.toEnum 200002) :: _ YearMonth)
      ((Enum.toEnum 200001) >>= Enum.succ)
    Assert.equal
      ((Enum.toEnum 200101) :: _ YearMonth)
      ((Enum.toEnum 200012) >>= Enum.succ)
    Assert.equal
      ((Enum.toEnum 200001) :: _ YearMonth)
      ((Enum.toEnum 200002) >>= Enum.pred)
    Assert.equal
      ((Enum.toEnum 199912) :: _ YearMonth)
      ((Enum.toEnum 200001) >>= Enum.pred)

  test "Eq YearMonth" do
    pure unit

  test "Ord YearMonth" do
    Assert.assert
      "YearMonth 2000-01 < WeekYear 2000-02"
      (((Enum.toEnum 200001) :: _ YearMonth) <
        ((Enum.toEnum 200002) :: _ YearMonth))
    Assert.assert
      "YearMonth 2000-02 < WeekYear 2001-01"
      (((Enum.toEnum 200002) :: _ YearMonth) <
        ((Enum.toEnum 200101) :: _ YearMonth))

  test "Show YearMonth" do
    Assert.equal
      (Just "(YearMonth (Year 2000) January)")
      (show <$> ((Enum.toEnum 200001) :: _ YearMonth))

  test "firstDateOfMonth" do
    Assert.equal
      do
        y <- Enum.toEnum 2000
        m <- Enum.toEnum 1
        d <- Enum.toEnum 1
        Date.exactDate y m d
      do
        y <- Enum.toEnum 2000
        m <- Enum.toEnum 1
        d <- Enum.toEnum 2
        date <- Date.exactDate y m d
        pure (YearMonth.firstDateOfMonth (YearMonth.fromDate date))

  test "firstDayOfMonth" do
    Assert.equal
      ((Enum.toEnum 1) :: _ Day) -- --01
      do
        y <- Enum.toEnum 2000
        m <- Enum.toEnum 1
        d <- Enum.toEnum 2
        date <- Date.exactDate y m d
        pure (YearMonth.firstDayOfMonth (YearMonth.fromDate date))

  test "fromDate / toDate" do
    let
      d1Maybe = do
        y <- Enum.toEnum 2000
        m <- Enum.toEnum 1
        d <- Enum.toEnum 2
        Date.exactDate y m d
      d1 = Unsafe.unsafePartial (Maybe.fromJust d1Maybe)
    Assert.equal
      (Just d1)
      (YearMonth.toDate (Date.day d1) (YearMonth.fromDate d1))

  test "lastDateOfMonth" do
    Assert.equal
      do
        y <- Enum.toEnum 2000
        m <- Enum.toEnum 1
        d <- Enum.toEnum 31
        Date.exactDate y m d
      do
        y <- Enum.toEnum 2000
        m <- Enum.toEnum 1
        d <- Enum.toEnum 2
        date <- Date.exactDate y m d
        pure (YearMonth.lastDateOfMonth (YearMonth.fromDate date))

  test "lastDayOfMonth" do
    Assert.equal
      ((Enum.toEnum 31) :: _ Day) -- --31
      do
        y <- Enum.toEnum 2000
        m <- Enum.toEnum 1
        d <- Enum.toEnum 2
        date <- Date.exactDate y m d
        pure (YearMonth.lastDayOfMonth (YearMonth.fromDate date))
    Assert.equal
      ((Enum.toEnum 29) :: _ Day) -- --29 (2000 is leap year)
      do
        y <- Enum.toEnum 2000
        m <- Enum.toEnum 2
        d <- Enum.toEnum 1
        date <- Date.exactDate y m d
        pure (YearMonth.lastDayOfMonth (YearMonth.fromDate date))

  test "yearMonth" do
    y <- pure (Enum.toEnum 2000)
    m <- pure (Enum.toEnum 1)
    d <- pure (Enum.toEnum 2)
    dateMaybe <-
      pure do
        y' <- y
        m' <- m
        d' <- d
        Date.exactDate y' m' d'
    date <- pure (Unsafe.unsafePartial (Maybe.fromJust dateMaybe))
    ym <- pure (YearMonth.yearMonth <$> y <*> m)
    -- TODO: Eq YearMonth
    Assert.equal (Just date) (ym >>= (YearMonth.toDate (Date.day date)))
