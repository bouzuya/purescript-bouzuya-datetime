module Bouzuya.DateTime.Date.YearMonth
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
import Data.Enum (class BoundedEnum, class Enum)
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (class Bounded, class Eq, class Ord, class Show, bind, bottom, mod, negate, otherwise, pure, show, top, (&&), (*), (+), (/), (/=), (<=), (<>))

data YearMonth = YearMonth Year Month

instance boundedYearMonth :: Bounded YearMonth where
  bottom = YearMonth bottom bottom
  top = YearMonth top top

instance boundedEnumYearMonth :: BoundedEnum YearMonth where
  cardinality = Enum.Cardinality 6570960
  toEnum n
    | ((-271820) * 100 + 1) <= n && n <= (275759 * 100 + 12) = do
        y <- Enum.toEnum (n / 100)
        m <- Enum.toEnum (n `mod` 100)
        pure (YearMonth y m)
    | otherwise = Nothing
  fromEnum (YearMonth y m) = (Enum.fromEnum y) * 100 + (Enum.fromEnum m)

instance enumYearMonth :: Enum YearMonth where
  succ (YearMonth y m)
    | m /= top = do
        m' <- Enum.succ m
        pure (YearMonth y m')
    | otherwise = do
        y' <- Enum.succ y
        pure (YearMonth y' bottom)
  pred (YearMonth y m)
    | m /= bottom = do
        m' <- Enum.pred m
        pure (YearMonth y m')
    | otherwise = do
        y' <- Enum.pred y
        pure (YearMonth y' top)

derive instance eqYearMonth :: Eq YearMonth

derive instance ordYearMonth :: Ord YearMonth

instance showYearMonth :: Show YearMonth where
  show (YearMonth y m) = "(YearMonth " <> show y <> " " <> show m <> ")"

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
