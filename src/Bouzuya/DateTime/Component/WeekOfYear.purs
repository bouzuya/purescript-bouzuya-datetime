module Bouzuya.DateTime.Component.WeekOfYear
  ( WeekOfYear
  , firstWeekOfYear
  , lastWeekOfYear
  , firstWeekdayOfYear
  , weekOfYear
  , weekYear
  ) where

import Bouzuya.DateTime.Component.DayOfYear (dayOfYear)
import Data.Date (Date, Month(..), Weekday(..), Year, exactDate, isLeapYear, weekday, year)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum, pred, succ, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (class Bounded, class Eq, class Ord, class Show, join, map, otherwise, show, (&&), (+), (-), (/), (<), (<$>), (<<<), (<=), (<>), (==), (>), (||))

data WeekDate = WeekDate Year WeekOfYear Weekday

newtype WeekOfYear = WeekOfYear Int

derive newtype instance eqWeekOfYear :: Eq WeekOfYear

derive newtype instance ordWeekOfYear :: Ord WeekOfYear

instance boundedWeekOfYear :: Bounded WeekOfYear where
  bottom = WeekOfYear 1
  top = WeekOfYear 53

instance boundedEnumWeekOfYear :: BoundedEnum WeekOfYear where
  cardinality = Cardinality 53
  toEnum n
    | 1 <= n && n <= 53 = Just (WeekOfYear n)
    | otherwise = Nothing
  fromEnum (WeekOfYear n) = n

instance enumWeekOfYear :: Enum WeekOfYear where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance showWeekOfYear :: Show WeekOfYear where
  show (WeekOfYear n) = "(WeekOfYear " <> show n <> ")"

firstWeekOfYear :: Year -> WeekOfYear
firstWeekOfYear _ = WeekOfYear 1

firstWeekdayOfYear :: Year -> Weekday
firstWeekdayOfYear y =
  let m = join (map (exactDate y January) (toEnum 1))
  in weekday (unsafePartial (fromJust m))

lastWeekOfYear :: Year -> WeekOfYear
lastWeekOfYear y =
  let
    w = firstWeekdayOfYear y
    p = (w == Thursday) || (w == Wednesday && isLeapYear y)
  in
    WeekOfYear (52 + if p then 1 else 0)

toWeekDate :: Date -> WeekDate
toWeekDate d =
  let
    y = year d
    w = weekday d
    d0104 = unsafePartial (fromJust (join (exactDate y January <$> (toEnum 4))))
    d1228 = unsafePartial (fromJust (join (exactDate y December <$> (toEnum 28))))
    doy = dayOfYear d
  in
    if doy < dayOfYear d0104 && weekday d > weekday d0104 then
      let py = unsafePartial (fromJust (pred y))
      in WeekDate py (lastWeekOfYear py) w
    else if doy > dayOfYear d1228 && weekday d < weekday d1228 then
      let ny = unsafePartial (fromJust (succ y))
      in WeekDate ny (firstWeekOfYear ny) w
    else
      let woy = WeekOfYear (((fromEnum doy) + (fromEnum (weekday d0104)) - 4 - 1) / 7 + 1)
      in WeekDate y woy w

weekOfYear :: Date -> WeekOfYear
weekOfYear d =
  let (WeekDate _ w _) = toWeekDate d
  in w

weekYear :: Date -> Year
weekYear d =
  let (WeekDate y _ _) = toWeekDate d
  in y
