module Bouzuya.OrdinalDate.Component.DayOfYear
  ( DayOfYear
  , firstDayOfYear
  , lastDayOfYear
  ) where

import Data.Date (Year)
import Data.Date as Date
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (class Bounded, class Eq, class Ord, class Show, bottom, otherwise, show, (&&), (+), (-), (<<<), (<=), (<>))

newtype DayOfYear = DayOfYear Int

derive newtype instance eqDayOfYear :: Eq DayOfYear

derive newtype instance ordDayOfYear :: Ord DayOfYear

instance boundedDayOfYear :: Bounded DayOfYear where
  bottom = DayOfYear 1
  top = DayOfYear 366

instance boundedEnumDayOfYear :: BoundedEnum DayOfYear where
  cardinality = Cardinality 366
  toEnum n
    | 1 <= n && n <= 366 = Just (DayOfYear n)
    | otherwise = Nothing
  fromEnum (DayOfYear n) = n

instance enumDayOfYear :: Enum DayOfYear where
  succ = Enum.toEnum <<< (_ + 1) <<< Enum.fromEnum
  pred = Enum.toEnum <<< (_ - 1) <<< Enum.fromEnum

instance showDayOfYear :: Show DayOfYear where
  show (DayOfYear n) = "(DayOfYear " <> show n <> ")"

firstDayOfYear :: Year -> DayOfYear
firstDayOfYear _ = bottom

lastDayOfYear :: Year -> DayOfYear
lastDayOfYear y
  | Date.isLeapYear y = DayOfYear 366
  | otherwise = DayOfYear 365
