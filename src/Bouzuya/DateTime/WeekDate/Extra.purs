module Bouzuya.DateTime.WeekDate.Extra
  ( firstWeekdayOfYear
  , lastWeekdayOfYear
  ) where

import Bouzuya.DateTime.Date.Interval.Year as Year
import Data.Date (Weekday, Year)
import Data.Date as Date

firstWeekdayOfYear :: Year -> Weekday
firstWeekdayOfYear y = Date.weekday (Year.firstDate y)

lastWeekdayOfYear :: Year -> Weekday
lastWeekdayOfYear y = Date.weekday (Year.lastDate y)
