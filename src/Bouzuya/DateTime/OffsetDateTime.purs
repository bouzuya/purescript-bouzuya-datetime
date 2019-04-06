module Bouzuya.DateTime.OffsetDateTime
  ( OffsetDateTime
  , fromUTCDateTime
  , fromUTCDateTimeInUTC
  , fromLocalDateTime
  , inOffset
  , inUTC
  , timeZoneOffset
  , toLocalDateTime
  , toUTCDateTime
  ) where

import Prelude

import Bouzuya.DateTime.TimeZoneOffset (TimeZoneOffset)
import Bouzuya.DateTime.TimeZoneOffset as TimeZoneOffset
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Maybe (Maybe)
import Data.Time.Duration (Milliseconds)
import Data.Time.Duration as TimeDuration

newtype LocalDateTime = LocalDateTime DateTime

derive instance eqLocalDateTime :: Eq LocalDateTime

instance showLocalDateTime :: Show LocalDateTime where
  show (LocalDateTime dt) = "(LocalDateTime " <> show dt <> ")"

data OffsetDateTime = OffsetDateTime UTCDateTime TimeZoneOffset LocalDateTime

derive instance eqOffsetDateTime :: Eq OffsetDateTime

instance showOffsetDateTime :: Show OffsetDateTime where
  show (OffsetDateTime utc offset _) =
    "(OffsetDateTime " <> show utc <> " " <> show offset <> ")"

newtype UTCDateTime = UTCDateTime DateTime

derive instance eqUTCDateTime :: Eq UTCDateTime

instance showUTCDateTime :: Show UTCDateTime where
  show (UTCDateTime dt) = "(UTCDateTime " <> show dt <> ")"

fromLocalDateTime :: TimeZoneOffset -> DateTime -> Maybe OffsetDateTime
fromLocalDateTime offset dt = fromLocalDateTime' offset (LocalDateTime dt)

fromLocalDateTime' :: TimeZoneOffset -> LocalDateTime -> Maybe OffsetDateTime
fromLocalDateTime' offset local = do
  utc <- toUTCDateTime' offset local
  fromUTCDateTime' offset utc

fromUTCDateTime :: TimeZoneOffset -> DateTime -> Maybe OffsetDateTime
fromUTCDateTime offset dt = fromUTCDateTime' offset (UTCDateTime dt)

fromUTCDateTime' :: TimeZoneOffset -> UTCDateTime -> Maybe OffsetDateTime
fromUTCDateTime' offset utc = inOffset offset (fromUTCDateTimeInUTC' utc)

fromUTCDateTimeInUTC :: DateTime -> OffsetDateTime
fromUTCDateTimeInUTC dt = fromUTCDateTimeInUTC' (UTCDateTime dt)

fromUTCDateTimeInUTC' :: UTCDateTime -> OffsetDateTime
fromUTCDateTimeInUTC' (UTCDateTime dt) =
  OffsetDateTime (UTCDateTime dt) TimeZoneOffset.utc (LocalDateTime dt)

inOffset :: TimeZoneOffset -> OffsetDateTime -> Maybe OffsetDateTime
inOffset offset (OffsetDateTime utc _ _) = do
  local <- toLocalDateTime' offset utc
  pure (OffsetDateTime utc offset local)

inUTC :: OffsetDateTime -> OffsetDateTime
inUTC (OffsetDateTime (UTCDateTime dt) _ _) =
  OffsetDateTime (UTCDateTime dt) TimeZoneOffset.utc (LocalDateTime dt)

timeZoneOffset :: OffsetDateTime -> TimeZoneOffset
timeZoneOffset (OffsetDateTime _ offset _) = offset

toLocalDateTime :: OffsetDateTime -> DateTime
toLocalDateTime (OffsetDateTime _ _ (LocalDateTime dt)) = dt

toLocalDateTime' :: TimeZoneOffset -> UTCDateTime -> Maybe LocalDateTime
toLocalDateTime' offset (UTCDateTime dt) =
  map
    LocalDateTime
    (DateTime.adjust
      (TimeDuration.negateDuration
        (TimeZoneOffset.toDuration offset :: Milliseconds))
      dt)

toUTCDateTime :: OffsetDateTime -> DateTime
toUTCDateTime (OffsetDateTime (UTCDateTime dt) _ _) = dt

toUTCDateTime' :: TimeZoneOffset -> LocalDateTime -> Maybe UTCDateTime
toUTCDateTime' offset (LocalDateTime dt) =
  map
    UTCDateTime
    (DateTime.adjust
      (TimeZoneOffset.toDuration offset :: Milliseconds)
      dt)
