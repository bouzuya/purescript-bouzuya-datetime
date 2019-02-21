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
import Bouzuya.WeekDate.Component.WeekOfYear (WeekOfYear)
import Bouzuya.WeekDate.Component.WeekOfYear as WeekOfYear
import Bouzuya.WeekDate.Component.WeekYear (WeekYear)
import Data.Date (Date, Month(..), Weekday(..), Year)
import Data.Date as Date
import Data.Enum (class BoundedEnum)
import Data.Enum as Enum
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (bind, bottom, identity, negate, otherwise, pure, (&&), (*), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<=), (==), (>), (>>=))

data WeekDate = WeekDate WeekYear WeekOfYear Weekday

firstWeekOfWeekYear :: WeekYear -> WeekOfYear
firstWeekOfWeekYear _ = bottom

lastWeekOfWeekYear :: WeekYear -> WeekOfYear
lastWeekOfWeekYear wy =
  -- FIXME
  let y = Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum (Enum.fromEnum wy)))
  in WeekOfYear.lastWeekOfYear y

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
    dayOfYearOffset = case WeekOfYear.firstWeekdayOfYear y of
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

week :: WeekDate -> WeekOfYear
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
