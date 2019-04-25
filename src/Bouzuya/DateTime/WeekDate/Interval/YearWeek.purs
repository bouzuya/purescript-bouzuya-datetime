module Bouzuya.DateTime.WeekDate.Interval.YearWeek
  ( YearWeek
  , firstWeekDate
  , fromWeekDate
  , lastWeekDate
  , toWeekDate
  , yearWeek
  ) where

import Prelude

import Bouzuya.DateTime.WeekDate (Week, WeekDate, WeekYear)
import Bouzuya.DateTime.WeekDate as WeekDate
import Bouzuya.DateTime.WeekDate.Internal as Internal
import Data.Date (Weekday)
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum as Enum
import Data.Maybe (Maybe)
import Data.Maybe as Maybe

data YearWeek = YearWeek WeekYear Week

instance boundedYearWeek :: Bounded YearWeek where
  bottom = fromWeekDate (bottom :: WeekDate)
  top = fromWeekDate (top :: WeekDate)

instance boundedEnumYearWeek :: BoundedEnum YearWeek where
  cardinality = Enum.Cardinality 28571357
  fromEnum (YearWeek wy w) = (Enum.fromEnum wy) * 100 + (Enum.fromEnum w)
  toEnum n
    | between bottomAsInt topAsInt n = do
        wy <- Enum.toEnum (n / 100)
        w <- Enum.toEnum (n `mod` 100)
        yearWeek wy w
    | otherwise = Maybe.Nothing

instance enumYearWeek :: Enum YearWeek where
  succ (YearWeek wy w)
    | wy == (top :: WeekYear) = Maybe.Nothing
    | w /= (Internal.lastWeekOfWeekYear wy) = do
        w' <- Enum.succ w
        pure (YearWeek wy w')
    | otherwise = do
        wy' <- Enum.succ wy
        pure (YearWeek wy' (bottom :: Week))
  pred (YearWeek wy w)
    | wy == (bottom :: WeekYear) = Maybe.Nothing
    | w /= (bottom :: Week) = do
        w' <- Enum.pred w
        pure (YearWeek wy w')
    | otherwise = do
        wy' <- Enum.pred wy
        pure (YearWeek wy' (Internal.lastWeekOfWeekYear wy'))

derive instance eqYearWeek :: Eq YearWeek

derive instance ordYearWeek :: Ord YearWeek

instance showYearWeek :: Show YearWeek where
  show (YearWeek wy w) = "(YearWeek " <> show wy <> " " <> show w <> ")"

bottomAsInt :: Int
bottomAsInt =
  ((Enum.fromEnum (bottom :: WeekYear)) * 100) +
    (Enum.fromEnum (bottom :: Week))

firstWeekDate :: YearWeek -> Maybe WeekDate
firstWeekDate (YearWeek wy w) = WeekDate.weekDate wy w (bottom :: Weekday)

fromWeekDate :: WeekDate -> YearWeek
fromWeekDate wd = YearWeek (WeekDate.weekYear wd) (WeekDate.week wd)

lastWeekDate :: YearWeek -> Maybe WeekDate
lastWeekDate (YearWeek wy w) = WeekDate.weekDate wy w (top :: Weekday)

toWeekDate :: Weekday -> YearWeek -> Maybe WeekDate
toWeekDate dow (YearWeek wy w) = WeekDate.weekDate wy w dow

topAsInt :: Int
topAsInt =
  ((Enum.fromEnum (top :: WeekYear)) * 100) +
    (Enum.fromEnum (Internal.lastWeekOfWeekYear (top :: WeekYear)))

yearWeek :: WeekYear -> Week -> Maybe YearWeek
yearWeek wy w
  | (wy == (bottom :: WeekYear)) &&
    (w /= (Internal.lastWeekOfWeekYear wy)) = Maybe.Nothing
  | (wy == (top :: WeekYear)) && (w /= bottom) = Maybe.Nothing
  | w > (Internal.lastWeekOfWeekYear wy) = Maybe.Nothing
  | otherwise = Maybe.Just (YearWeek wy w)
