module Bouzuya.Date.YearMonth
  ( YearMonth
  , fromDate
  , toDate
  , yearMonth
  ) where

import Data.Date (Date, Day, Month, Year)
import Data.Date as Date
import Data.Maybe (Maybe)

data YearMonth = YearMonth Year Month

fromDate :: Date -> YearMonth
fromDate date = YearMonth (Date.year date) (Date.month date)

toDate :: Day -> YearMonth -> Maybe Date
toDate day (YearMonth y m) = Date.exactDate y m day

yearMonth :: Year -> Month -> YearMonth
yearMonth = YearMonth
