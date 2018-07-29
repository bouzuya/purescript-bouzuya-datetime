module Bouzuya.DateTime.Component.WeekOfYear
  ( WeekOfYear
  , startWeekdayOfYear
  , weekOfYear
  , weeksInYear
  ) where

import Bouzuya.DateTime.Component.DayOfYear (dayOfYear)
import Data.Date (Date, Month(..), Weekday(..), Year, exactDate, isLeapYear, weekday, year)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum, pred, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (class Bounded, class Eq, class Ord, class Show, join, map, otherwise, show, (&&), (+), (-), (/), (<), (<$>), (<<<), (<=), (<>), (==), (>), (||))

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

startWeekdayOfYear :: Year -> Weekday
startWeekdayOfYear y =
  let m = join (map (exactDate y January) (toEnum 1))
  in
  weekday (unsafePartial (fromJust m))

weekOfYear :: Date -> WeekOfYear
weekOfYear d =
  let
    y = year d
    d0104 = unsafePartial (fromJust (join (exactDate y January <$> (toEnum 4))))
    d1228 = unsafePartial (fromJust (join (exactDate y December <$> (toEnum 28))))
    doy = dayOfYear d
  in
    if doy < dayOfYear d0104 && weekday d > weekday d0104 then
      WeekOfYear (weeksInYear (unsafePartial (fromJust (pred y)))) -- prev year
    else if doy > dayOfYear d1228 && weekday d < weekday d1228 then
      WeekOfYear 1 -- next year
    else
      WeekOfYear (((fromEnum doy) + (fromEnum (weekday d0104)) - 4 - 1) / 7 + 1)

weeksInYear :: Year -> Int
weeksInYear y =
  let
    w = startWeekdayOfYear y
    p = (w == Thursday) || (w == Wednesday && isLeapYear y)
  in
    52 + if p then 1 else 0
