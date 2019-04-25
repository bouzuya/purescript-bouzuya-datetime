module Test.Bouzuya.DateTime.WeekDate.Interval.YearWeek
  ( tests
  ) where

import Prelude

import Bouzuya.DateTime.WeekDate (WeekDate)
import Bouzuya.DateTime.WeekDate as WeekDate
import Bouzuya.DateTime.WeekDate.Interval.YearWeek (YearWeek)
import Bouzuya.DateTime.WeekDate.Interval.YearWeek as YearWeek
import Data.Enum as Enum
import Data.Maybe as Maybe
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "Bouzuya.DateTime.WeekDate.Interval.YearWeek" do
  TestUnit.test "Bounded YearWeek" do
    Assert.equal (YearWeek.fromWeekDate bottom) (bottom :: YearWeek)
    Assert.equal (YearWeek.fromWeekDate top) (top :: YearWeek)
    Assert.assert "bottom <= top" ((bottom :: YearWeek) <= (top :: YearWeek))

  -- let
  --   last :: forall t. BoundedEnum t => Lazy t
  --   last = Lazy.defer \_ -> ST.run do
  --     valueRef <- STRef.new (bottom :: t)
  --     let (Enum.Cardinality c) = cardinality :: _ t
  --     ST.for 1 c \_ -> do -- cardinality - 1 times
  --       value <- STRef.read valueRef
  --       Maybe.maybe'
  --         (\_ -> Unsafe.unsafeCrashWith "nooo")
  --         (\value' -> STRef.write value' valueRef)
  --         (Enum.succ value)
  --     STRef.read valueRef

  -- TestUnit.test "BoundedEnum Year" do
  --   Assert.equal (top :: Year) last

  -- TestUnit.test "BoundedEnum YearMonth" do
  --   Assert.equal (top :: YearMonth) last

  -- TestUnit.test "BoundedEnum WeekYear" do
  --   Assert.equal (top :: WeekYear) last

  -- TestUnit.test "BoundedEnum Week" do
  --   Assert.equal (top :: Week) last

  -- TestUnit.test "BoundedEnum YearWeek (cardinality)" do
  --   Assert.equal (top :: YearWeek) last

  TestUnit.test "BoundedEnum YearWeek" do
    Assert.equal
      (Maybe.Just (bottom :: YearWeek))
      (Enum.toEnum (Enum.fromEnum (bottom :: YearWeek)))
    Assert.equal
      (Maybe.Just (top :: YearWeek))
      (Enum.toEnum (Enum.fromEnum (top :: YearWeek)))

  TestUnit.test "Enum YearWeek" do
    Assert.equal Maybe.Nothing (Enum.pred (bottom :: YearWeek))
    Assert.equal Maybe.Nothing (Enum.succ (top :: YearWeek))
    Assert.equal
      ((Enum.toEnum 200002) :: _ YearWeek)
      ((Enum.toEnum 200001) >>= Enum.succ)
    Assert.equal
      ((Enum.toEnum 200101) :: _ YearWeek)
      ((Enum.toEnum 200052) >>= Enum.succ)
    Assert.equal
      ((Enum.toEnum 200001) :: _ YearWeek)
      ((Enum.toEnum 200002) >>= Enum.pred)
    Assert.equal
      ((Enum.toEnum 199952) :: _ YearWeek)
      ((Enum.toEnum 200001) >>= Enum.pred)
    Assert.equal
      ((Enum.toEnum 200501) :: _ YearWeek)
      ((Enum.toEnum 200453) >>= Enum.succ)
    Assert.equal
      ((Enum.toEnum 200453) :: _ YearWeek)
      ((Enum.toEnum 200501) >>= Enum.pred)

  TestUnit.test "Eq YearWeek" do
    pure unit

  TestUnit.test "Ord YearWeek" do
    Assert.assert
      "YearWeek 2000-01 < WeekYear 2000-02"
      (((Enum.toEnum 200001) :: _ YearWeek) <
        ((Enum.toEnum 200002) :: _ YearWeek))
    Assert.assert
      "YearMonth 2000-02 < WeekYear 2001-01"
      (((Enum.toEnum 200002) :: _ YearWeek) <
        ((Enum.toEnum 200101) :: _ YearWeek))

  TestUnit.test "Show YearWeek" do
    Assert.equal
      (Maybe.Just "(YearWeek (WeekYear 2000) (Week 1))")
      (show <$> ((Enum.toEnum 200001) :: _ YearWeek))

  TestUnit.test "firstWeekDate" do
    let yw1 = bottom :: YearWeek
    Assert.equal Maybe.Nothing (YearWeek.firstWeekDate yw1)
    let yw2 = Enum.succ (bottom :: YearWeek)
    Assert.equal
      (Maybe.Just "(WeekDate (WeekYear -271820) (Week 1) Monday)")
      (show <$> (yw2 >>= \yw -> YearWeek.firstWeekDate yw))
    let yw3 = top
    Assert.equal
      (Maybe.Just "(WeekDate (WeekYear 275760) (Week 1) Monday)")
      (show <$> (YearWeek.firstWeekDate yw3))

  TestUnit.test "fromWeekDate" do
    Assert.equal
      (bottom :: YearWeek)
      (YearWeek.fromWeekDate (bottom :: WeekDate))
    Assert.equal
      (top :: YearWeek)
      (YearWeek.fromWeekDate (top :: WeekDate))

  TestUnit.test "lastWeekDate" do
    let yw1 = bottom :: YearWeek
    Assert.equal
      (Maybe.Just "(WeekDate (WeekYear -271821) (Week 52) Sunday)")
      (show <$> (YearWeek.lastWeekDate yw1))
    let yw2 = Enum.pred top
    Assert.equal
      (Maybe.Just "(WeekDate (WeekYear 275759) (Week 52) Sunday)")
      (show <$> (yw2 >>= \yw -> YearWeek.lastWeekDate yw))
    let yw3 = top
    Assert.equal Maybe.Nothing (YearWeek.lastWeekDate yw3)

  TestUnit.test "toWeekDate" do
    let bottomWeekDate = bottom :: WeekDate
    Assert.equal
      (Maybe.Just bottomWeekDate)
      (YearWeek.toWeekDate
        (WeekDate.weekday bottomWeekDate)
        (YearWeek.fromWeekDate (bottomWeekDate)))
    let topWeekDate = top :: WeekDate
    Assert.equal
      (Maybe.Just topWeekDate)
      (YearWeek.toWeekDate
        (WeekDate.weekday topWeekDate)
        (YearWeek.fromWeekDate (topWeekDate)))

  TestUnit.test "yearWeek" do
    let
      wyMaybe = Enum.toEnum 2000
      wMaybe = Enum.toEnum 1
      dowMaybe = Enum.toEnum 2
      wdMaybe = join (WeekDate.weekDate <$> wyMaybe <*> wMaybe <*> dowMaybe)
      ywMaybe = join (YearWeek.yearWeek <$> wyMaybe <*> wMaybe)
    Assert.equal true (Maybe.isJust wdMaybe)
    Assert.equal wdMaybe do
      ym <- ywMaybe
      wd <- wdMaybe
      YearWeek.toWeekDate (WeekDate.weekday wd) ym
