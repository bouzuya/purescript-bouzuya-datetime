module Bouzuya.DateTime.OrdinalDate.Component.DayOfYear
  ( DayOfYear
  , lastDayOfYearOfLongYear
  , lastDayOfYearOfShortYear
  ) where

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (class Bounded, class Eq, class Ord, class Show, otherwise, show, (&&), (+), (-), (<<<), (<=), (<>))

newtype DayOfYear = DayOfYear Int

instance boundedDayOfYear :: Bounded DayOfYear where
  bottom = DayOfYear 1
  top = DayOfYear 366

instance boundedEnumDayOfYear :: BoundedEnum DayOfYear where
  cardinality = Cardinality 366
  fromEnum (DayOfYear n) = n
  toEnum n
    | 1 <= n && n <= 366 = Just (DayOfYear n)
    | otherwise = Nothing

instance enumDayOfYear :: Enum DayOfYear where
  pred = Enum.toEnum <<< (_ - 1) <<< Enum.fromEnum
  succ = Enum.toEnum <<< (_ + 1) <<< Enum.fromEnum

derive newtype instance eqDayOfYear :: Eq DayOfYear

derive newtype instance ordDayOfYear :: Ord DayOfYear

instance showDayOfYear :: Show DayOfYear where
  show (DayOfYear n) = "(DayOfYear " <> show n <> ")"

lastDayOfYearOfLongYear :: DayOfYear
lastDayOfYearOfLongYear = DayOfYear 366

lastDayOfYearOfShortYear :: DayOfYear
lastDayOfYearOfShortYear = DayOfYear 365
