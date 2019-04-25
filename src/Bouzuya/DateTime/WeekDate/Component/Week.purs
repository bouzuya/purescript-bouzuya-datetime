module Bouzuya.DateTime.WeekDate.Component.Week
  ( Week
  , lastWeekOfLongYear
  , lastWeekOfShortYear
  ) where

import Prelude

import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum as Enum
import Data.Maybe (Maybe(..))

newtype Week = Week Int

instance boundedWeek :: Bounded Week where
  bottom = Week bottomAsInt
  top = Week topAsInt

instance boundedEnumWeek :: BoundedEnum Week where
  cardinality = Enum.Cardinality 53
  fromEnum (Week n) = n
  toEnum n
    | between bottomAsInt topAsInt n = Just (Week n)
    | otherwise = Nothing

instance enumWeek :: Enum Week where
  pred = Enum.toEnum <<< (_ - 1) <<< Enum.fromEnum
  succ = Enum.toEnum <<< (_ + 1) <<< Enum.fromEnum

derive newtype instance eqWeek :: Eq Week

derive newtype instance ordWeek :: Ord Week

instance showWeek :: Show Week where
  show (Week n) = "(Week " <> show n <> ")"

bottomAsInt :: Int
bottomAsInt = 1

lastWeekOfLongYear :: Week
lastWeekOfLongYear = Week 53

lastWeekOfShortYear :: Week
lastWeekOfShortYear = Week 52

topAsInt :: Int
topAsInt = 53
