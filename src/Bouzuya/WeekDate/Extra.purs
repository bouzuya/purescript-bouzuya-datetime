module Bouzuya.WeekDate.Extra
  ( firstWeekdayOfYear
  , lastWeekdayOfYear
  ) where


import Bouzuya.Date.Extra as DateExtra
import Data.Date (Weekday, Year)
import Data.Date as Date

firstWeekdayOfYear :: Year -> Weekday
firstWeekdayOfYear y = Date.weekday (DateExtra.firstDateOfYear y)

lastWeekdayOfYear :: Year -> Weekday
lastWeekdayOfYear y = Date.weekday (DateExtra.lastDateOfYear y)
