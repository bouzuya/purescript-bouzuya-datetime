module Bouzuya.OrdinalDate.Component.DayOfYear
  ( DayOfYear
  , dayOfYear
  , firstDayOfYear
  , lastDayOfYear
  ) where

import Data.Date (Date, Year, diff, exactDate, isLeapYear, year)
import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum, toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromJust)
import Data.Time.Duration (Days(..))
import Partial.Unsafe (unsafePartial)
import Prelude (class Bounded, class Eq, class Ord, class Show, bottom, otherwise, show, (&&), (+), (-), (<<<), (<=), (<>), (>>=))

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
