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
import Data.Date (Date, Year)
import Data.Date as Date
import Data.Enum (class Enum)
import Data.Enum as Enum
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (class Bounded, class Eq, class Ord, class Show, bottom, map, show, top, (<$>), (<*>), (<>), (==))

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

fromDate :: Date -> OrdinalDate
fromDate d = OrdinalDate (Date.year d) (DayOfYear.dayOfYear d)

ordinalDate :: Year -> DayOfYear -> Maybe OrdinalDate
ordinalDate y doy =
  map fromDate (DayOfYear.exactDateFromDayOfYear y doy)

toDate :: OrdinalDate -> Date
toDate (OrdinalDate y doy) =
  Unsafe.unsafePartial
    (Maybe.fromJust
      (DayOfYear.exactDateFromDayOfYear y doy))

year :: OrdinalDate -> Year
year (OrdinalDate y _) = y
