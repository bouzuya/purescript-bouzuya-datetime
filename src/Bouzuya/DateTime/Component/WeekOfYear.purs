module Bouzuya.DateTime.Component.WeekOfYear
  ( WeekOfYear
  ) where

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum, toEnum)
import Data.Maybe (Maybe(..))
import Prelude (class Bounded, class Eq, class Ord, class Show, otherwise, show, (&&), (+), (-), (<<<), (<=), (<>))

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
