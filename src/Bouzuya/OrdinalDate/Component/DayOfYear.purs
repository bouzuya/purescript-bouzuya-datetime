module Bouzuya.OrdinalDate.Component.DayOfYear
  ( DayOfYear
  , dayOfYear
  , exactDateFromDayOfYear
  , firstDayOfYear
  , lastDayOfYear
  ) where

import Data.Date (Date, Day, Month(..), Year, diff, exactDate, isLeapYear, lastDayOfMonth, year)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), enumFromTo, fromEnum, toEnum)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Time.Duration (Days(..))
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafePartial)
import Prelude (class Bounded, class Eq, class Ord, class Show, bottom, otherwise, show, (&&), (+), (-), (<<<), (<=), (<>), (==), (>>=))

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
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance showDayOfYear :: Show DayOfYear where
  show (DayOfYear n) = "(DayOfYear " <> show n <> ")"

dayOfYear :: Date -> DayOfYear
dayOfYear d =
  let
    (Days n) = diff d (startOfYear d)
    days = Days (Int.toNumber ((Int.floor n) + 1))
  in
    unsafePartial (fromJust (fromDays days))

exactDateFromDayOfYear :: Year -> DayOfYear -> Maybe Date
exactDateFromDayOfYear  y d@(DayOfYear n)
  | lastDayOfYear y == (DayOfYear 365) && n == 366 = Nothing
  | otherwise =
    fst
      (foldl
        (\a@(Tuple r s) i ->
          case r of
            Just _ -> a
            Nothing ->
              let
                s' = s - (fromEnum (lastDayOfMonth y i))
              in
                Tuple
                  (if s' <= 0
                    then Just (unsafeDate y i (unsafeDay s))
                    else Nothing)
                  s')
        (Tuple Nothing n)
        (enumFromTo January December :: Array Month))

firstDayOfYear :: Year -> DayOfYear
firstDayOfYear _ = bottom

lastDayOfYear :: Year -> DayOfYear
lastDayOfYear y
  | isLeapYear y = DayOfYear 366
  | otherwise = DayOfYear 365

-- month :: Year -> DayOfYear -> Month -- canonical?
-- day :: Year -> DayOfYear -> Day -- canonical?

fromDays :: Days -> Maybe DayOfYear
fromDays (Days n) = Int.fromNumber n >>= toEnum

startOfYear :: Date -> Date
startOfYear d = unsafePartial (fromJust (exactDate (year d) bottom bottom))

toDays :: DayOfYear -> Days
toDays (DayOfYear n) = Days (Int.toNumber n)

unsafeDate :: Year -> Month -> Day -> Date
unsafeDate y m d = unsafePartial (fromJust (exactDate y m d))

unsafeDay :: Int -> Day
unsafeDay n = unsafePartial (fromJust (toEnum n))
