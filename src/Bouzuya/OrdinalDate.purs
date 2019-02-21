module Bouzuya.OrdinalDate
  ( OrdinalDate
  , dayOfYear
  , fromDate
  , ordinalDate
  , year
  , toDate
  ) where

import Bouzuya.OrdinalDate.Component.DayOfYear (DayOfYear)
import Bouzuya.OrdinalDate.Component.DayOfYear as DayOfYear
import Data.Array as Array
import Data.Date (Date, Month, Year)
import Data.Date as Date
import Data.Enum (class Enum)
import Data.Enum as Enum
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Time.Duration (Days(..))
import Partial.Unsafe as Unsafe
import Prelude (class Bounded, class Eq, class Ord, class Show, bind, bottom, map, otherwise, show, top, (-), (<$>), (<*>), (<<<), (<>), (==), (>), (>>=))

data OrdinalDate = OrdinalDate Year DayOfYear

instance boundedOrdinalDate :: Bounded OrdinalDate where
  bottom = OrdinalDate bottom bottom
  top = OrdinalDate top (DayOfYear.lastDayOfYear top)

instance enumOrdinalDate :: Enum OrdinalDate where
  pred (OrdinalDate y doy) =
    if doy == DayOfYear.firstDayOfYear y
    then
      let py = Enum.pred y
      in OrdinalDate <$> py <*> (DayOfYear.lastDayOfYear <$> py)
    else OrdinalDate y <$> (Enum.pred doy)
  succ (OrdinalDate y doy) =
    if doy == DayOfYear.lastDayOfYear y
    then
      let ny = Enum.succ y
      in OrdinalDate <$> ny <*> (DayOfYear.firstDayOfYear <$> ny)
    else OrdinalDate y <$> (Enum.succ doy)

derive instance eqOrdinalDate :: Eq OrdinalDate

derive instance ordOrdinalDate :: Ord OrdinalDate

instance showOrdinalDate :: Show OrdinalDate where
  show (OrdinalDate y doy) = "(OrdinalDate " <> show y <> " " <> show doy <> ")"

dayOfYear :: OrdinalDate -> DayOfYear
dayOfYear (OrdinalDate _ doy) = doy

dayOfYearFromDate :: Date -> DayOfYear
dayOfYearFromDate d =
  let
    (Days n) = Date.diff d (firstDateOfYear (Date.year d))
    doy = (Int.fromNumber n) >>= Enum.succ >>= Enum.toEnum
  in Unsafe.unsafePartial (Maybe.fromJust doy)

exactDateFromDayOfYear :: Year -> DayOfYear -> Maybe Date
exactDateFromDayOfYear y doy
  | doy > (DayOfYear.lastDayOfYear y) = Nothing
  | otherwise =
      Array.findMap
        (\m -> do
          d <- Enum.toEnum ((Enum.fromEnum doy) - (daysBeforeMonth y m))
          Date.exactDate y m d)
        ((Enum.enumFromTo bottom top) :: Array Month)
      where
        daysBeforeMonth :: Year -> Month -> Int
        daysBeforeMonth y' m =
          Maybe.maybe
            0
            (\m' ->
              Foldable.sum
                (map
                  (Enum.fromEnum <<< (Date.lastDayOfMonth y'))
                  ((Enum.enumFromTo bottom m') :: Array Month)))
            (Enum.pred m)

firstDateOfYear :: Year -> Date
firstDateOfYear y =
  Unsafe.unsafePartial (Maybe.fromJust (Date.exactDate y bottom bottom))

fromDate :: Date -> OrdinalDate
fromDate d = OrdinalDate (Date.year d) (dayOfYearFromDate d)

ordinalDate :: Year -> DayOfYear -> Maybe OrdinalDate
ordinalDate y doy =
  map fromDate (exactDateFromDayOfYear y doy)

toDate :: OrdinalDate -> Date
toDate (OrdinalDate y doy) =
  Unsafe.unsafePartial (Maybe.fromJust (exactDateFromDayOfYear y doy))

year :: OrdinalDate -> Year
year (OrdinalDate y _) = y
