module Bouzuya.WeekDate
  ( WeekDate
  , fromDate
  , toDate
  , week
  , weekYear
  , weekday
  ) where

import Bouzuya.OrdinalDate (OrdinalDate)
import Bouzuya.OrdinalDate as OrdinalDate
import Bouzuya.OrdinalDate.Component.DayOfYear as DayOfYear
import Bouzuya.WeekDate.Component.Week (Week)
import Bouzuya.WeekDate.Component.Week as Week
import Bouzuya.WeekDate.Component.WeekYear (WeekYear)
import Data.Date (Date, Month(..), Weekday(..), Year)
import Data.Date as Date
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum as Enum
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (class Bounded, class Eq, class Ord, class Show, bind, bottom, identity, negate, otherwise, pure, show, top, (&&), (*), (+), (-), (/), (/=), (<), (<$>), (<*>), (<<<), (<=), (<>), (==), (>), (>>=))

data WeekDate = WeekDate WeekYear Week Weekday

instance boundedWeekDate :: Bounded WeekDate where
  bottom =
    Unsafe.unsafePartial
      (Maybe.fromJust do
        y <- Enum.succ bottom
        d <- Date.exactDate y bottom bottom
        fromDate d)
  top =
    Unsafe.unsafePartial
      (Maybe.fromJust do
        y <- Enum.pred top
        d <- Date.exactDate y top top
        fromDate d)

instance enumWeekDate :: Enum WeekDate where
  pred (WeekDate wy w wday)
    | wday /= bottom = WeekDate wy w <$> Enum.pred wday
    | w /= firstWeekOfWeekYear wy = WeekDate wy <$> (Enum.pred w) <*> (pure top)
    | otherwise = do
        pwy <- Enum.pred wy
        pw <- pure (lastWeekOfWeekYear pwy)
        pwday <- pure top
        pure (WeekDate pwy pw pwday)
  succ (WeekDate wy w wday)
    | wday /= top = WeekDate wy w <$> Enum.succ wday
    | w /= lastWeekOfWeekYear wy =
        WeekDate wy <$> (Enum.succ w) <*> (pure bottom)
    | otherwise = do
        nwy <- Enum.succ wy
        nw <- pure (firstWeekOfWeekYear nwy)
        nwday <- pure bottom
        pure (WeekDate nwy nw nwday)

derive instance eqWeekDate :: Eq WeekDate

derive instance ordWeekDate :: Ord WeekDate

instance showWeekDate :: Show WeekDate where
  show (WeekDate wy w wday) =
    "(WeekDate " <> show wy <> " " <> show w <> " " <> show wday <> ")"

firstWeekOfWeekYear :: WeekYear -> Week
firstWeekOfWeekYear _ = bottom

lastWeekOfWeekYear :: WeekYear -> Week
lastWeekOfWeekYear wy =
  -- safe
  let y = Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum (Enum.fromEnum wy)))
  in Week.lastWeekOfYear y

fromDate :: Date -> Maybe WeekDate
fromDate d = do
  let
    y = Date.year d
    d0104 =
      Unsafe.unsafePartial
        (Maybe.fromJust
          (Date.exactDate y January <$> (Enum.toEnum 4) >>= identity))
  wy <- weekYearFromDate d
  wy' <- Enum.toEnum (Enum.fromEnum y)
  woy <-
    if wy < wy' then pure (lastWeekOfWeekYear wy)
      else if wy > wy' then pure (firstWeekOfWeekYear wy)
      else
        let
          doyN = Enum.fromEnum (OrdinalDate.dayOfYear (OrdinalDate.fromDate d))
          woyN = (doyN + (Enum.fromEnum (Date.weekday d0104)) - 4 - 1) / 7 + 1
      in Enum.toEnum woyN
  pure (WeekDate wy woy (Date.weekday d))

modifyBoundedEnum :: forall a. BoundedEnum a => (Int -> Int) -> a -> Maybe a
modifyBoundedEnum f = Enum.toEnum <<< f <<< Enum.fromEnum

ordinalDate0104 :: Year -> OrdinalDate
ordinalDate0104 y =
  let
    doy0104 = modifyBoundedEnum (_ + 3) (DayOfYear.firstDayOfYear y)
    od0104Maybe = (OrdinalDate.ordinalDate y) <$> doy0104 >>= identity
  in Unsafe.unsafePartial (Maybe.fromJust od0104Maybe) -- safe

ordinalDate1228 :: Year -> OrdinalDate
ordinalDate1228 y =
  let
    doy1228 = modifyBoundedEnum (_ - 3) (DayOfYear.lastDayOfYear y)
    od1228Maybe = (OrdinalDate.ordinalDate y) <$> doy1228 >>= identity
  in Unsafe.unsafePartial (Maybe.fromJust od1228Maybe) -- safe

toDate :: WeekDate -> Date
toDate (WeekDate wy woy wday) =
  let
    y =
      Unsafe.unsafePartial
        (Maybe.fromJust (Enum.toEnum (Enum.fromEnum wy))) -- FIXME
    w = Enum.fromEnum woy
    dayOfYearOffset = case Week.firstWeekdayOfYear y of
      Monday -> 0
      Tuesday -> -1
      Wednesday -> -2
      Thursday -> -3
      Friday -> 3
      Saturday -> 2
      Sunday -> 1
    dayOfYear = (w - 1) * 7 + (Enum.fromEnum wday) + dayOfYearOffset
    lastDayOfYear = Enum.fromEnum <<< DayOfYear.lastDayOfYear
    ordinalDateMaybe =
      if dayOfYear <= 0
      then do
        py <- Enum.pred y
        py' <- Enum.toEnum (Enum.fromEnum py) -- FIXME
        doy <- Enum.toEnum ((lastDayOfYear py) + dayOfYear)
        OrdinalDate.ordinalDate py' doy
      else if dayOfYear > lastDayOfYear y
      then do
        ny <- Enum.succ y
        ny' <- Enum.toEnum (Enum.fromEnum ny) -- FIXME
        doy <- Enum.toEnum (dayOfYear - (lastDayOfYear y))
        OrdinalDate.ordinalDate ny' doy
      else
        OrdinalDate.ordinalDate <$> (pure y) <*> (Enum.toEnum dayOfYear) >>= identity
    dateMaybe = OrdinalDate.toDate <$> ordinalDateMaybe
  in Unsafe.unsafePartial (Maybe.fromJust dateMaybe)

week :: WeekDate -> Week
week (WeekDate _ w _) = w

weekYear :: WeekDate -> WeekYear
weekYear (WeekDate wy _ _) = wy

weekYearFromDate :: Date -> Maybe WeekYear
weekYearFromDate d
  | Date.month d == January =
      let
        y = Date.year d
        d0104 =
          Unsafe.unsafePartial
            (Maybe.fromJust
              (Date.exactDate y January <$> (Enum.toEnum 4) >>= identity))
      in
        if ((Date.day d) < (Date.day d0104)) &&
          ((Date.weekday d) > (Date.weekday d0104))
        then (Enum.fromEnum <$> Enum.pred y) >>= Enum.toEnum
        else Enum.toEnum (Enum.fromEnum y)
  | Date.month d == December =
      let
        y = Date.year d
        d1228 =
          Unsafe.unsafePartial
            (Maybe.fromJust
              (Date.exactDate y December <$> (Enum.toEnum 28) >>= identity))
      in
        if ((Date.day d) > (Date.day d1228)) &&
          ((Date.weekday d) < (Date.weekday d1228))
        then (Enum.fromEnum <$> Enum.succ y) >>= Enum.toEnum -- TODO
        else Enum.toEnum (Enum.fromEnum y)
  | otherwise = Enum.toEnum (Enum.fromEnum (Date.year d))

weekday :: WeekDate -> Weekday
weekday (WeekDate _ _ wday) = wday
