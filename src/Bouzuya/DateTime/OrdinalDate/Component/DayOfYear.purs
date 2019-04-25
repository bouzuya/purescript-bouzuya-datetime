module Bouzuya.DateTime.OrdinalDate.Component.DayOfYear
  ( DayOfYear
  , lastDayOfYearOfLongYear
  , lastDayOfYearOfShortYear
  ) where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as Enum
import Data.Maybe (Maybe(..))

newtype DayOfYear = DayOfYear Int

instance boundedDayOfYear :: Bounded DayOfYear where
  bottom = DayOfYear bottomAsInt
  top = DayOfYear topAsInt

instance boundedEnumDayOfYear :: BoundedEnum DayOfYear where
  cardinality = Cardinality 366
  fromEnum (DayOfYear n) = n
  toEnum n
    | between bottomAsInt topAsInt n = Just (DayOfYear n)
    | otherwise = Nothing

instance enumDayOfYear :: Enum DayOfYear where
  pred = Enum.toEnum <<< (_ - 1) <<< Enum.fromEnum
  succ = Enum.toEnum <<< (_ + 1) <<< Enum.fromEnum

derive newtype instance eqDayOfYear :: Eq DayOfYear

derive newtype instance ordDayOfYear :: Ord DayOfYear

instance showDayOfYear :: Show DayOfYear where
  show (DayOfYear n) = "(DayOfYear " <> show n <> ")"

bottomAsInt :: Int
bottomAsInt = 1

lastDayOfYearOfLongYear :: DayOfYear
lastDayOfYearOfLongYear = DayOfYear 366

lastDayOfYearOfShortYear :: DayOfYear
lastDayOfYearOfShortYear = DayOfYear 365

topAsInt :: Int
topAsInt = 366
