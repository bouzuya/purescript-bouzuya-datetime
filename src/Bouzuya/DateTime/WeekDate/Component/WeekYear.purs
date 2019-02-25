module Bouzuya.DateTime.WeekDate.Component.WeekYear
  ( WeekYear
  ) where

import Data.Date (Year)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Data.Ord as Ord
import Prelude (class Bounded, class Eq, class Ord, class Show, bottom, otherwise, show, top, (&&), (+), (-), (<<<), (<=), (<>))

newtype WeekYear = WeekYear Int

bottomAsInt :: Int
bottomAsInt = ((Enum.fromEnum (bottom :: Year)) - 1) -- -271821

topAsInt :: Int
topAsInt = ((Enum.fromEnum (top :: Year)) + 1) -- 275760

instance boundedWeekYear :: Bounded WeekYear where
  bottom = WeekYear bottomAsInt
  top = WeekYear topAsInt

instance boundedEnumWeekYear :: BoundedEnum WeekYear where
  cardinality = Cardinality ((Ord.abs bottomAsInt) + 1 + topAsInt)
  toEnum n
    | bottomAsInt <= n && n <= topAsInt = Just (WeekYear n)
    | otherwise = Nothing
  fromEnum (WeekYear n) = n

instance enumWeekYear :: Enum WeekYear where
  succ = Enum.toEnum <<< (_ + 1) <<< Enum.fromEnum
  pred = Enum.toEnum <<< (_ - 1) <<< Enum.fromEnum

derive instance eqWeekYear :: Eq WeekYear

derive instance ordWeekYear :: Ord WeekYear

instance showWeekYear :: Show WeekYear where
  show (WeekYear n) = "(WeekYear " <> show n <> ")"
