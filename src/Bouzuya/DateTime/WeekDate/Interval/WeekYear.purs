module Bouzuya.DateTime.WeekDate.Interval.WeekYear
  ( firstWeekDate -- start week date
  , lastWeekDate  -- end week date
  ) where

import Prelude

import Bouzuya.DateTime.WeekDate (WeekDate, WeekYear)
import Bouzuya.DateTime.WeekDate as WeekDate
import Bouzuya.DateTime.WeekDate.Internal as Internal
import Data.Maybe (Maybe)

firstWeekDate :: WeekYear -> Maybe WeekDate
firstWeekDate wy = WeekDate.weekDate wy bottom bottom

lastWeekDate :: WeekYear -> Maybe WeekDate
lastWeekDate wy = WeekDate.weekDate wy (Internal.lastWeekOfWeekYear wy) top
