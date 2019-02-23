module Bouzuya.WeekDate
  ( WeekDate
  , firstWeekDateOfWeek
  , firstWeekDateOfWeekYear
  , fromDate
  , lastWeekDateOfWeek
  , lastWeekDateOfWeekYear
  , toDate
  , week
  , weekDate
  , weekYear
  , weekday
  ) where

import Bouzuya.OrdinalDate as OrdinalDate
import Bouzuya.WeekDate.Component.Week (Week)
import Bouzuya.WeekDate.Component.WeekYear (WeekYear)
import Bouzuya.WeekDate.Extra as WeekDateExtra
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
  bottom =
    Unsafe.unsafePartial
      (Maybe.fromJust do
        y <- Enum.succ bottom
        d <- Date.exactDate y bottom bottom
        fromDate d)
  top =
    Unsafe.unsafePartial
      (Maybe.fromJust do
        y <- Enum.pred top
        d <- Date.exactDate y top top
        fromDate d)

instance enumWeekDate :: Enum WeekDate where
  pred (WeekDate wy w wday)
    | wday /= bottom = WeekDate wy w <$> Enum.pred wday
    | w /= firstWeekOfWeekYear wy = WeekDate wy <$> (Enum.pred w) <*> (pure top)
    | otherwise = do
        pwy <- Enum.pred wy
        pw <- pure (lastWeekOfWeekYear pwy)
        pwday <- pure top
        pure (WeekDate pwy pw pwday)
  succ (WeekDate wy w wday)
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

firstWeekDateOfWeek :: WeekYear -> Week -> WeekDate
firstWeekDateOfWeek wy w = WeekDate wy w bottom

firstWeekDateOfWeekYear :: WeekYear -> WeekDate
firstWeekDateOfWeekYear wy = WeekDate wy bottom bottom

firstWeekOfWeekYear :: WeekYear -> Week
firstWeekOfWeekYear _ = bottom

lastWeekDateOfWeek :: WeekYear -> Week -> WeekDate
lastWeekDateOfWeek wy w = WeekDate wy w top

lastWeekDateOfWeekYear :: WeekYear -> WeekDate
lastWeekDateOfWeekYear wy = WeekDate wy (lastWeekOfWeekYear wy) top

lastWeekOfWeekYear :: WeekYear -> Week
lastWeekOfWeekYear wy =
  -- safe
  let y = Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum (Enum.fromEnum wy)))
  in lastWeekOfYear y
  where
    isLongYear :: Year -> Boolean
    isLongYear y =
      let w = WeekDateExtra.firstWeekdayOfYear y
      in (w == Date.Thursday) || (w == Date.Wednesday && Date.isLeapYear y)

    lastWeekOfYear :: Year -> Week
    lastWeekOfYear y
      | isLongYear y = Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum 53))
      | otherwise = Unsafe.unsafePartial (Maybe.fromJust (Enum.toEnum 52))

fromDate :: Date -> Maybe WeekDate
fromDate d = do
  let
    y = Date.year d
    d0104 =
      Unsafe.unsafePartial
        (Maybe.fromJust
          (Date.exactDate y January <$> (Enum.toEnum 4) >>= identity))
  wy <- weekYearFromDate d
  wy' <- Enum.toEnum (Enum.fromEnum y)
  woy <-
    if wy < wy' then pure (lastWeekOfWeekYear wy)
      else if wy > wy' then pure (firstWeekOfWeekYear wy)
      else
        let
          doyN = Enum.fromEnum (OrdinalDate.dayOfYear (OrdinalDate.fromDate d))
          woyN = (doyN + (Enum.fromEnum (Date.weekday d0104)) - 4 - 1) / 7 + 1
      in Enum.toEnum woyN
  pure (WeekDate wy woy (Date.weekday d))

toDate :: WeekDate -> Date
toDate (WeekDate wy w wday)
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
weekDate wy w wday
  | w > lastWeekOfWeekYear wy = Nothing
  | otherwise = Just (WeekDate wy w wday)

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
        then (Enum.fromEnum <$> Enum.pred y) >>= Enum.toEnum
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
        then (Enum.fromEnum <$> Enum.succ y) >>= Enum.toEnum
        else Enum.toEnum (Enum.fromEnum y)
  | otherwise = Enum.toEnum (Enum.fromEnum (Date.year d))

weekday :: WeekDate -> Weekday
weekday (WeekDate _ _ wday) = wday
