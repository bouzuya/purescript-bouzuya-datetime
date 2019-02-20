module Bouzuya.OrdinalDate
  ( OrdinalDate
  , fromDate
  , toDate
  ) where

import Bouzuya.DateTime (DayOfYear)
import Bouzuya.DateTime.Component.DayOfYear as DayOfYear
import Data.Date (Date, Year)
import Data.Date as Date
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe

data OrdinalDate = OrdinalDate Year DayOfYear

fromDate :: Date -> OrdinalDate
fromDate d = OrdinalDate (Date.year d) (DayOfYear.dayOfYear d)

toDate :: OrdinalDate -> Date
toDate (OrdinalDate y doy) =
  Unsafe.unsafePartial
    (Maybe.fromJust
      (DayOfYear.exactDateFromDayOfYear y doy))
