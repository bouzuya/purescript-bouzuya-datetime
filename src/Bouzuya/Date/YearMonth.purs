module Bouzuya.Date.YearMonth
  ( YearMonth
  , firstDateOfMonth
  , firstDayOfMonth
  , fromDate
  , lastDateOfMonth
  , lastDayOfMonth
  , toDate
  , yearMonth
  ) where

import Data.Date (Date, Day, Month, Year)
import Data.Date as Date
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (bottom)

data YearMonth = YearMonth Year Month

firstDateOfMonth :: YearMonth -> Date
firstDateOfMonth (YearMonth y m) =
  Unsafe.unsafePartial (Maybe.fromJust (Date.exactDate y m bottom))

firstDayOfMonth :: YearMonth -> Day
firstDayOfMonth _ = bottom

fromDate :: Date -> YearMonth
fromDate date = YearMonth (Date.year date) (Date.month date)

lastDateOfMonth :: YearMonth -> Date
lastDateOfMonth (YearMonth y m) =
  Unsafe.unsafePartial
    (Maybe.fromJust (Date.exactDate y m (Date.lastDayOfMonth y m)))

lastDayOfMonth :: YearMonth -> Day
lastDayOfMonth (YearMonth y m) = Date.lastDayOfMonth y m

toDate :: Day -> YearMonth -> Maybe Date
toDate day (YearMonth y m) = Date.exactDate y m day

yearMonth :: Year -> Month -> YearMonth
yearMonth = YearMonth
