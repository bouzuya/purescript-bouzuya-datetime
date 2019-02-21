module Bouzuya.WeekDate.Component.Week
  ( Week
  , firstWeekOfYear
  , firstWeekdayOfYear
  , lastWeekOfYear
  , lastWeekdayOfYear
  , week
  , weekYear
  ) where

import Bouzuya.Date.Extra as DateExtra
import Bouzuya.OrdinalDate as OrdinalDate
import Data.Date (Date, Weekday(..), Year, exactDate, isLeapYear, weekday, year)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum, pred, succ, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Prelude (class Bounded, class Eq, class Ord, class Show, bind, bottom, identity, join, negate, otherwise, pure, show, top, (&&), (*), (+), (-), (/), (<), (<$>), (<*>), (<<<), (<=), (<>), (==), (>), (>>=), (||))

data WeekDate = WeekDate Year Week Weekday

newtype Week = Week Int

derive newtype instance eqWeek :: Eq Week

derive newtype instance ordWeek :: Ord Week

longYearLastWeek :: Week
longYearLastWeek = Week 53

shortYearLastWeek :: Week
shortYearLastWeek = Week 52

instance boundedWeek :: Bounded Week where
  bottom = Week 1
  top = longYearLastWeek

instance boundedEnumWeek :: BoundedEnum Week where
  cardinality = Cardinality 53
  toEnum n
    | 1 <= n && n <= 53 = Just (Week n)
    | otherwise = Nothing
  fromEnum (Week n) = n

instance enumWeek :: Enum Week where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance showWeek :: Show Week where
  show (Week n) = "(Week " <> show n <> ")"

firstWeekOfYear :: Year -> Week
firstWeekOfYear _ = bottom

firstWeekdayOfYear :: Year -> Weekday
firstWeekdayOfYear y = weekday (DateExtra.firstDateOfYear y)

lastWeekOfYear :: Year -> Week
lastWeekOfYear y =
  let
    w = firstWeekdayOfYear y
    isLongYear = (w == Thursday) || (w == Wednesday && isLeapYear y)
  in
    if isLongYear then longYearLastWeek else shortYearLastWeek

lastWeekdayOfYear :: Year -> Weekday
lastWeekdayOfYear y = weekday (DateExtra.lastDateOfYear y)

toDate :: WeekDate -> Date
toDate (WeekDate y (Week w) d) =
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
      let woy = Week (((fromEnum doy) + (fromEnum (weekday d0104)) - 4 - 1) / 7 + 1)
      in WeekDate y woy w

week :: Date -> Week
week d =
  let (WeekDate _ w _) = toWeekDate d
  in w

weekYear :: Date -> Year
weekYear d =
  let (WeekDate y _ _) = toWeekDate d
  in y
