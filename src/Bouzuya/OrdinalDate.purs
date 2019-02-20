module Bouzuya.OrdinalDate
  ( OrdinalDate
  , dayOfYear
  , fromDate
  , ordinalDate
  , year
  , toDate
  ) where

import Bouzuya.DateTime (DayOfYear)
import Bouzuya.DateTime.Component.DayOfYear as DayOfYear
import Data.Date (Date, Year)
import Data.Date as Date
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (class Eq, class Show, map, show, (<>))

data OrdinalDate = OrdinalDate Year DayOfYear

derive instance eqOrdinalDate :: Eq OrdinalDate

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
