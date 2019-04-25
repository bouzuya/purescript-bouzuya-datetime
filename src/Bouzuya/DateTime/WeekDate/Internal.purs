module Bouzuya.DateTime.WeekDate.Internal
  ( lastWeekOfWeekYear
  ) where

import Prelude

import Bouzuya.DateTime.Date.Extra as DateExtra
import Bouzuya.DateTime.WeekDate.Component.Week (Week)
import Bouzuya.DateTime.WeekDate.Component.Week as Week
import Bouzuya.DateTime.WeekDate.Component.WeekYear (WeekYear)
import Data.Date as Date
import Data.Enum as Enum
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe

lastWeekOfWeekYear :: WeekYear -> Week
lastWeekOfWeekYear wy =
  let
    { firstWeekday, isLeapYear } =
      -- -271821 is not leap year
      -- -271821-01-01 is Friday
      -- -271821-12-31 is Friday
      if wy == bottom then { firstWeekday: Date.Friday, isLeapYear: false }
      -- 275760 is leap year
      -- 275760-01-01 is Tuesday
      -- 275760-12-31 is Wednesday
      else if wy == top then { firstWeekday: Date.Tuesday, isLeapYear: true }
      else
        let
          y =
            Unsafe.unsafePartial
              (Maybe.fromJust (Enum.toEnum (Enum.fromEnum wy)))
        in
          { firstWeekday: Date.weekday (DateExtra.firstDateOfYear y)
          , isLeapYear: Date.isLeapYear y
          }
    isLongYear =
      (firstWeekday == Date.Thursday) ||
        ((firstWeekday == Date.Wednesday) && isLeapYear)
  in
    if isLongYear then Week.lastWeekOfLongYear else Week.lastWeekOfShortYear
