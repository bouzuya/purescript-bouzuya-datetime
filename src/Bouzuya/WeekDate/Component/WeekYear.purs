module Bouzuya.WeekDate.Component.WeekYear
  ( WeekYear
  ) where

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..))
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (class Bounded, class Eq, class Ord, class Show, negate, otherwise, show, (&&), (+), (-), (<<<), (<=), (<>))

newtype WeekYear = WeekYear Int

instance boundedWeekYear :: Bounded WeekYear where
  bottom = WeekYear (-271821) -- -271821 == (fromEnum (bottom :: Year)) - 1
  top = WeekYear 275760       -- 275760 == (fromEnum (top :: Year)) + 1

instance boundedEnumWeekYear :: BoundedEnum WeekYear where
  cardinality = Cardinality 547582
  toEnum n
    | (-271821) <= n && n <= 275760 = Just (WeekYear n)
    | otherwise = Nothing
  fromEnum (WeekYear n) = n

instance enumWeekYear :: Enum WeekYear where
  succ = Enum.toEnum <<< (_ + 1) <<< Enum.fromEnum
  pred = Enum.toEnum <<< (_ - 1) <<< Enum.fromEnum

derive instance eqWeekYear :: Eq WeekYear

derive instance ordWeekYear :: Ord WeekYear

instance showWeekYear :: Show WeekYear where
  show (WeekYear n) = "(WeekYear " <> show n <> ")"
