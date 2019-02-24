module Bouzuya.WeekDate
  ( WeekDate
  , firstWeekDateOfWeek
  , firstWeekDateOfWeekYear
  , fromDate
  , lastWeekDateOfWeek
  , lastWeekDateOfWeekYear
  , module ReExportWeek
  , module ReExportWeekYear
  , toDate
  , week
  , weekDate
  , weekYear
  , weekday
  ) where

import Bouzuya.Date.Extra as DateExtra
import Bouzuya.OrdinalDate as OrdinalDate
import Bouzuya.WeekDate.Component.Week (Week)
import Bouzuya.WeekDate.Component.Week (Week) as ReExportWeek
import Bouzuya.WeekDate.Component.Week as Week
import Bouzuya.WeekDate.Component.WeekYear (WeekYear)
import Bouzuya.WeekDate.Component.WeekYear (WeekYear) as ReExportWeekYear
import Data.Date (Date, Month(..), Weekday, Year)
import Data.Date as Date
import Data.Enum (class Enum)
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (class Bounded, class Eq, class Ord, class Show, bind, bottom, identity, otherwise, pure, show, top, ($), (&&), (*), (+), (-), (/), (/=), (<), (<$>), (<*>), (<=), (<>), (==), (>), (>>=), (||))

data WeekDate = WeekDate WeekYear Week Weekday

instance boundedWeekDate :: Bounded WeekDate where
  bottom = fromDate (bottom :: Date)
  top = fromDate (top :: Date)

instance enumWeekDate :: Enum WeekDate where
  pred wd@(WeekDate wy w wday)
    | wd == bottom = Nothing
    | wday /= bottom = WeekDate wy w <$> Enum.pred wday
    | w /= firstWeekOfWeekYear wy = WeekDate wy <$> (Enum.pred w) <*> (pure top)
    | otherwise = do
        pwy <- Enum.pred wy
        pw <- pure (lastWeekOfWeekYear pwy)
        pwday <- pure top
        pure (WeekDate pwy pw pwday)
  succ wd@(WeekDate wy w wday)
    | wd == top = Nothing
    | wday /= top = WeekDate wy w <$> Enum.succ wday
    | w /= lastWeekOfWeekYear wy =
        WeekDate wy <$> (Enum.succ w) <*> (pure bottom)
    | otherwise = do
        nwy <- Enum.succ wy
        nw <- pure (firstWeekOfWeekYear nwy)
        nwday <- pure bottom
        pure (WeekDate nwy nw nwday)

derive instance eqWeekDate :: Eq WeekDate

derive instance ordWeekDate :: Ord WeekDate

instance showWeekDate :: Show WeekDate where
  show (WeekDate wy w wday) =
    "(WeekDate " <> show wy <> " " <> show w <> " " <> show wday <> ")"

date0104 :: Year -> Date
date0104 y =
  Unsafe.unsafePartial
    (Maybe.fromJust ((Enum.toEnum 4) >>= (Date.exactDate y bottom)))

firstWeekDateOfWeek :: WeekYear -> Week -> Maybe WeekDate
firstWeekDateOfWeek wy w
  | wy == bottom = Nothing
  | wy == top && w /= (firstWeekOfWeekYear wy) = Nothing
  | w > (lastWeekOfWeekYear wy) = Nothing
  | otherwise = Just (WeekDate wy w bottom)

firstWeekDateOfWeekYear :: WeekYear -> Maybe WeekDate
firstWeekDateOfWeekYear wy
  | wy == bottom = Nothing
  | otherwise = Just (WeekDate wy bottom bottom)

firstWeekOfWeekYear :: WeekYear -> Week
firstWeekOfWeekYear _ = bottom

fromDate :: Date -> WeekDate
fromDate d
  -- -271821-W52
  --     Mon Tue Wed Thu Fri Sat Sun
  -- W52  27  28  29  30  31   1   2
  -- W01   3   4   5   6   7   8   9
  -- (day d) < (day d0104) && (weekday d) > (weekday d0104)
  | ((Date.year d) == bottom) &&
    ((Date.month d) == bottom) &&
    ((Just (Date.day d)) < (Enum.toEnum 3)) =
      WeekDate bottom top (Date.weekday d)

  -- 275760-W01
  --     Mon Tue Wed Thu Fri Sat Sun
  -- W52  24  25  26  27  28  29  30
  -- W01  31   1   2   3   4   5   6
  -- (day d) > (day d1228) && (weekday d) < (weekday d1228)
  | d == top = WeekDate top bottom Date.Monday

  | otherwise = Unsafe.unsafePartial $ Maybe.fromJust do
      let
        y = Date.year d
        d0104 = date0104 y
        wy = Unsafe.unsafePartial (Maybe.fromJust (weekYearFromDate d))
      wy' <- Enum.toEnum (Enum.fromEnum y)
      woy <-
        if wy < wy' then pure (lastWeekOfWeekYear wy)
          else if wy > wy' then pure (firstWeekOfWeekYear wy)
          else
            let
              doyN =
                Enum.fromEnum (OrdinalDate.dayOfYear (OrdinalDate.fromDate d))
              woyN =
                (doyN + (Enum.fromEnum (Date.weekday d0104)) - 4 - 1) / 7 + 1
          in Enum.toEnum woyN
      pure (WeekDate wy woy (Date.weekday d))

lastWeekDateOfWeek :: WeekYear -> Week -> Maybe WeekDate
lastWeekDateOfWeek wy w
  | wy == bottom && w /= (lastWeekOfWeekYear wy) = Nothing
  | wy == top = Nothing
  | w > (lastWeekOfWeekYear wy) = Nothing
  | otherwise = Just (WeekDate wy w top)

lastWeekDateOfWeekYear :: WeekYear -> Maybe WeekDate
lastWeekDateOfWeekYear wy
  | wy == top = Nothing
  | otherwise = Just (WeekDate wy (lastWeekOfWeekYear wy) top)

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

toDate :: WeekDate -> Date
toDate (WeekDate wy w wday)
  | wy == bottom =
      let
        dayMaybe =
          if wday == Date.Saturday
          then Enum.toEnum 1
          else Enum.toEnum 2
      in
        Unsafe.unsafePartial
          (Maybe.fromJust
            (dayMaybe >>= Date.exactDate bottom bottom))

  | wy == top = top

  -- 1 Mon, 2 Tue, 3 Wed, 4 Thu, 5 Fri, 6 Sat, 7 Sun
  --    29,    30,    31,     1,     2,     3,     4 (-01-04 -> 7 Sun)
  --    30,    31,     1,     2,     3,     4,     5 (-01-04 -> 6 Sat)
  --    31,     1,     2,     3,     4,     5,     6 (-01-04 -> 5 Fri)
  --     1,     2,     3,     4,     5,     6,     7 (-01-04 -> 4 Thu)
  --     2,     3,     4,     5,     6,     7,     8 (-01-04 -> 3 Wed)
  --     3,     4,     5,     6,     7,     8,     9 (-01-04 -> 2 Tue)
  --     4,     5,     6,     7,     8,     9,    10 (-01-04 -> 1 Mon)
  | w == firstWeekOfWeekYear wy = Unsafe.unsafePartial $ Maybe.fromJust $ do
      wyn <- pure (Enum.fromEnum wy)
      cy <- Enum.toEnum wyn
      py <- Enum.toEnum (wyn - 1)
      wdayn <- pure (Enum.fromEnum wday)
      wdayn0104 <- do
        day04 <- Enum.toEnum 4
        d0104 <- Date.exactDate cy bottom day04
        pure (Enum.fromEnum (Date.weekday d0104))
      offset <- pure (wdayn - wdayn0104 + 4)
      y <- pure (if offset <= 0 then py else cy)
      m <- pure (if offset <= 0 then top else bottom)
      day <- Enum.toEnum (offset + (if offset <= 0 then 31 else 0))
      Date.exactDate y m day

  -- 1 Mon, 2 Tue, 3 Wed, 4 Thu, 5 Fri, 6 Sat, 7 Sun
  --    22,    23,    24,    25,    26,    27,    28 (-12-28 -> 7 Sun)
  --    23,    24,    25,    26,    27,    28,    29 (-12-28 -> 6 Sat)
  --    24,    25,    26,    27,    28,    29,    30 (-12-28 -> 5 Fri)
  --    25,    26,    27,    28,    29,    30,    31 (-12-28 -> 4 Thu)
  --    26,    27,    28,    29,    30,    31,     1 (-12-28 -> 3 Wed)
  --    27,    28,    29,    30,    31,     1,     2 (-12-28 -> 2 Tue)
  --    28,    29,    30,    31,     1,     2,     3 (-12-28 -> 1 Mon)
  | w == lastWeekOfWeekYear wy = Unsafe.unsafePartial $ Maybe.fromJust $ do
      wyn <- pure (Enum.fromEnum wy)
      cy <- Enum.toEnum wyn
      ny <- Enum.toEnum (wyn + 1)
      wdayn <- pure (Enum.fromEnum wday)
      wdayn1228 <- do
        day28 <- Enum.toEnum 28
        d1228 <- Date.exactDate cy top day28
        pure (Enum.fromEnum (Date.weekday d1228))
      offset <- pure (wdayn - wdayn1228 - 3)
      y <- pure (if offset <= 0 then cy else ny)
      m <- pure (if offset <= 0 then top else bottom)
      day <- Enum.toEnum (offset + (if offset <= 0 then 31 else 0))
      Date.exactDate y m day

  | otherwise = Unsafe.unsafePartial $ Maybe.fromJust $ do
      y <- Enum.toEnum (Enum.fromEnum wy)
      wn <- pure (Enum.fromEnum w)
      wdayn <- pure (Enum.fromEnum wday)
      wdayn0104 <- do
        day04 <- Enum.toEnum 4
        d0104 <- Date.exactDate y bottom day04
        pure (Enum.fromEnum (Date.weekday d0104))
      doyn <- pure ((wn - 1) * 7 + wdayn - wdayn0104 + 4)
      doy <- Enum.toEnum doyn
      od <- OrdinalDate.ordinalDate y doy
      pure (OrdinalDate.toDate od)

week :: WeekDate -> Week
week (WeekDate _ w _) = w

weekDate :: WeekYear -> Week -> Weekday -> Maybe WeekDate
weekDate wy w dow
  | wy == bottom && (w /= top || dow < Date.Saturday) = Nothing
  | wy == top && (w /= bottom || Date.Monday < dow) = Nothing
  | w > lastWeekOfWeekYear wy = Nothing
  | otherwise = Just (WeekDate wy w dow)

weekYear :: WeekDate -> WeekYear
weekYear (WeekDate wy _ _) = wy

weekYearFromDate :: Date -> Maybe WeekYear
weekYearFromDate d
  | Date.month d == January =
      let
        y = Date.year d
        d0104 =
          Unsafe.unsafePartial
            (Maybe.fromJust
              (Date.exactDate y January <$> (Enum.toEnum 4) >>= identity))
      in
        if ((Date.day d) < (Date.day d0104)) &&
          ((Date.weekday d) > (Date.weekday d0104))
        then Enum.toEnum ((Enum.fromEnum y) - 1)
        else Enum.toEnum (Enum.fromEnum y)
  | Date.month d == December =
      let
        y = Date.year d
        d1228 =
          Unsafe.unsafePartial
            (Maybe.fromJust
              (Date.exactDate y December <$> (Enum.toEnum 28) >>= identity))
      in
        if ((Date.day d) > (Date.day d1228)) &&
          ((Date.weekday d) < (Date.weekday d1228))
        then Enum.toEnum ((Enum.fromEnum y) + 1)
        else Enum.toEnum (Enum.fromEnum y)
  | otherwise = Enum.toEnum (Enum.fromEnum (Date.year d))

weekday :: WeekDate -> Weekday
weekday (WeekDate _ _ wday) = wday
