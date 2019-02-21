module Bouzuya.WeekDate.Component.Week
  ( Week
  , firstWeekOfYear
  , lastWeekOfYear
  ) where

import Bouzuya.WeekDate.Extra as WeekDateExtra
import Data.Date (Weekday(..), Year)
import Data.Date as Date
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (class Bounded, class Eq, class Ord, class Show, bottom, otherwise, show, (&&), (+), (-), (<<<), (<=), (<>), (==), (||))

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
  succ = Enum.toEnum <<< (_ + 1) <<< Enum.fromEnum
  pred = Enum.toEnum <<< (_ - 1) <<< Enum.fromEnum

instance showWeek :: Show Week where
  show (Week n) = "(Week " <> show n <> ")"

firstWeekOfYear :: Year -> Week
firstWeekOfYear _ = bottom

lastWeekOfYear :: Year -> Week
lastWeekOfYear y =
  let
    w = WeekDateExtra.firstWeekdayOfYear y
    isLongYear = (w == Thursday) || (w == Wednesday && Date.isLeapYear y)
  in
    if isLongYear then longYearLastWeek else shortYearLastWeek
