module Bouzuya.WeekDate
  ( WeekDate
  , fromDate
  , toDate
  ) where

import Bouzuya.OrdinalDate (OrdinalDate)
import Bouzuya.OrdinalDate as OrdinalDate
import Bouzuya.OrdinalDate.Component.DayOfYear (DayOfYear)
import Bouzuya.OrdinalDate.Component.DayOfYear as DayOfYear
import Bouzuya.WeekDate.Component.WeekOfYear (WeekOfYear)
import Bouzuya.WeekDate.Component.WeekOfYear as WeekOfYear
import Data.Date (Date, Weekday(..), Year)
import Data.Date as Date
import Data.Enum (class BoundedEnum)
import Data.Enum as Enum
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (bind, identity, negate, pure, (&&), (*), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<=), (>), (>>=))

data WeekDate = WeekDate Year WeekOfYear Weekday

fromDate :: Date -> WeekDate
fromDate d =
  let
    y = Date.year d
    wday = Date.weekday d
    od = OrdinalDate.fromDate d
    od0104 = ordinalDate0104 y
    wday0104 = Date.weekday (OrdinalDate.toDate od0104)
    od1228 = ordinalDate1228 y
    wday1228 = Date.weekday (OrdinalDate.toDate od1228)
    dateMaybe =
      if od < od0104 && wday > wday0104 then do
        py <- Enum.pred y
        pure (WeekDate py (WeekOfYear.lastWeekOfYear py) wday)
      else if od > od1228 && wday < wday1228 then do
        ny <- Enum.succ y
        pure (WeekDate ny (WeekOfYear.firstWeekOfYear ny) wday)
      else do
        let
          doyInt =
            Enum.fromEnum (OrdinalDate.dayOfYear (OrdinalDate.fromDate d))
          woyInt = (doyInt + (Enum.fromEnum wday0104) - 4 - 1) / 7 + 1
        woy <- Enum.toEnum woyInt
        pure (WeekDate y woy wday)
    in Unsafe.unsafePartial (Maybe.fromJust dateMaybe)

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
toDate (WeekDate y woy wday) =
  let
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
        doy <- Enum.toEnum ((lastDayOfYear py) + dayOfYear)
        OrdinalDate.ordinalDate py doy
      else if dayOfYear > lastDayOfYear y
      then do
        ny <- Enum.succ y
        doy <- Enum.toEnum (dayOfYear - (lastDayOfYear y))
        OrdinalDate.ordinalDate ny doy
      else
        OrdinalDate.ordinalDate <$> (pure y) <*> (Enum.toEnum dayOfYear) >>= identity
    dateMaybe = OrdinalDate.toDate <$> ordinalDateMaybe
  in Unsafe.unsafePartial (Maybe.fromJust dateMaybe)
