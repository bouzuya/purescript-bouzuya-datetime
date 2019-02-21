module Bouzuya.DateTime.Component.WeekOfYear
  ( WeekOfYear
  , exactDateFromWeekOfYear
  , firstWeekOfYear
  , firstWeekdayOfYear
  , lastWeekOfYear
  , lastWeekdayOfYear
  , weekOfYear
  , weekYear
  ) where

import Bouzuya.OrdinalDate as OrdinalDate
import Data.Date (Date, Weekday(..), Year, exactDate, isLeapYear, weekday, year)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum, pred, succ, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (class Bounded, class Eq, class Ord, class Show, bind, bottom, identity, join, negate, otherwise, pure, show, top, (&&), (*), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<=), (<>), (==), (>), (>>=), (||))

data WeekDate = WeekDate Year WeekOfYear Weekday

newtype WeekOfYear = WeekOfYear Int

derive newtype instance eqWeekOfYear :: Eq WeekOfYear

derive newtype instance ordWeekOfYear :: Ord WeekOfYear

longYearLastWeekOfYear :: WeekOfYear
longYearLastWeekOfYear = WeekOfYear 53

shortYearLastWeekOfYear :: WeekOfYear
shortYearLastWeekOfYear = WeekOfYear 52

instance boundedWeekOfYear :: Bounded WeekOfYear where
  bottom = WeekOfYear 1
  top = longYearLastWeekOfYear

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

exactDateFromWeekOfYear :: Year -> WeekOfYear -> Weekday -> Maybe Date
exactDateFromWeekOfYear wy woy w
  | lastWeekOfYear wy < woy = Nothing
  | otherwise =
    let wd = WeekDate wy woy w
    in pure (toDate wd)

firstDateOfYear :: Year -> Date
firstDateOfYear y = unsafePartial (fromJust (exactDate y bottom bottom))

firstWeekOfYear :: Year -> WeekOfYear
firstWeekOfYear _ = bottom

firstWeekdayOfYear :: Year -> Weekday
firstWeekdayOfYear y = weekday (firstDateOfYear y)

lastDateOfYear :: Year -> Date
lastDateOfYear y = unsafePartial (fromJust (exactDate y top top))

lastWeekOfYear :: Year -> WeekOfYear
lastWeekOfYear y =
  let
    w = firstWeekdayOfYear y
    isLongYear = (w == Thursday) || (w == Wednesday && isLeapYear y)
  in
    if isLongYear then longYearLastWeekOfYear else shortYearLastWeekOfYear

lastWeekdayOfYear :: Year -> Weekday
lastWeekdayOfYear y = weekday (lastDateOfYear y)

toDate :: WeekDate -> Date
toDate (WeekDate y (WeekOfYear w) d) =
  let
    dayOfYearOffset = case firstWeekdayOfYear y of
      Monday -> 0
      Tuesday -> -1
      Wednesday -> -2
      Thursday -> -3
      Friday -> 3
      Saturday -> 2
      Sunday -> 1
    dayOfYear = (w - 1) * 7 + (fromEnum d) + dayOfYearOffset
    dayOfYearTop y' = if isLeapYear y' then 366 else 365
    ordinalDateMaybe =
      if dayOfYear <= 0
      then do
        py <- pred y
        doy <- toEnum ((dayOfYearTop py) + dayOfYear)
        OrdinalDate.ordinalDate py doy
      else if dayOfYear > dayOfYearTop y
      then do
        ny <- succ y
        doy <- toEnum (dayOfYear - (dayOfYearTop y))
        OrdinalDate.ordinalDate ny doy
      else
        OrdinalDate.ordinalDate <$> (pure y) <*> (toEnum dayOfYear) >>= identity
    dateMaybe = OrdinalDate.toDate <$> ordinalDateMaybe
  in
    unsafePartial (fromJust dateMaybe)

toWeekDate :: Date -> WeekDate
toWeekDate d =
  let
    y = year d
    w = weekday d
    d0104 = unsafePartial (fromJust (join (exactDate y bottom <$> (toEnum 4))))
    d1228 = unsafePartial (fromJust (join (exactDate y top <$> (toEnum 28))))
    dayOfYear = OrdinalDate.dayOfYear <<< OrdinalDate.fromDate
    doy = dayOfYear d
  in
    if doy < dayOfYear d0104 && weekday d > weekday d0104 then
      let py = unsafePartial (fromJust (pred y)) -- FIXME
      in WeekDate py (lastWeekOfYear py) w
    else if doy > dayOfYear d1228 && weekday d < weekday d1228 then
      let ny = unsafePartial (fromJust (succ y)) -- FIXME
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
