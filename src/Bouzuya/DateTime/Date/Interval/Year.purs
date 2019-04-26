module Bouzuya.DateTime.Date.Interval.Year
  ( firstDate -- start date
  , fromDate
  , lastDate  -- end date
  ) where

import Prelude

import Data.Date (Date, Year)
import Data.Date as Date
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe

firstDate :: Year -> Date
firstDate y =
  Unsafe.unsafePartial (Maybe.fromJust (Date.exactDate y bottom bottom))

fromDate :: Date -> Year
fromDate = Date.year

lastDate :: Year -> Date
lastDate y =
  Unsafe.unsafePartial (Maybe.fromJust (Date.exactDate y top top))
