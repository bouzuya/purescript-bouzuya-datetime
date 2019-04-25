module Bouzuya.DateTime.WeekDate.Component.WeekYear
  ( WeekYear
  ) where

import Prelude

import Data.Date (Year)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Data.Ord as Ord

newtype WeekYear = WeekYear Int

instance boundedWeekYear :: Bounded WeekYear where
  bottom = WeekYear bottomAsInt
  top = WeekYear topAsInt

instance boundedEnumWeekYear :: BoundedEnum WeekYear where
  cardinality = Cardinality ((Ord.abs bottomAsInt) + 1 + topAsInt)
  fromEnum (WeekYear n) = n
  toEnum n
    | between bottomAsInt topAsInt n = Just (WeekYear n)
    | otherwise = Nothing

instance enumWeekYear :: Enum WeekYear where
  succ = Enum.toEnum <<< (_ + 1) <<< Enum.fromEnum
  pred = Enum.toEnum <<< (_ - 1) <<< Enum.fromEnum

derive instance eqWeekYear :: Eq WeekYear

derive instance ordWeekYear :: Ord WeekYear

instance showWeekYear :: Show WeekYear where
  show (WeekYear n) = "(WeekYear " <> show n <> ")"

bottomAsInt :: Int
bottomAsInt = ((Enum.fromEnum (bottom :: Year)) - 1) -- -271821

topAsInt :: Int
topAsInt = ((Enum.fromEnum (top :: Year)) + 1) -- 275760
