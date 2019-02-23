module Bouzuya.WeekDate.Component.Week
  ( Week
  ) where

import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Prelude (class Bounded, class Eq, class Ord, class Show, otherwise, show, (&&), (+), (-), (<<<), (<=), (<>))

newtype Week = Week Int

derive newtype instance eqWeek :: Eq Week

derive newtype instance ordWeek :: Ord Week

instance boundedWeek :: Bounded Week where
  bottom = Week 1
  top = Week 53

instance boundedEnumWeek :: BoundedEnum Week where
  cardinality = Enum.Cardinality 53
  toEnum n
    | 1 <= n && n <= 53 = Just (Week n)
    | otherwise = Nothing
  fromEnum (Week n) = n

instance enumWeek :: Enum Week where
  succ = Enum.toEnum <<< (_ + 1) <<< Enum.fromEnum
  pred = Enum.toEnum <<< (_ - 1) <<< Enum.fromEnum

instance showWeek :: Show Week where
  show (Week n) = "(Week " <> show n <> ")"
