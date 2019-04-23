module Bouzuya.DateTime.TimeZoneOffset
  ( TimeZoneOffset
  , fromDuration
  , toDuration
  , utc
  ) where

import Prelude

import Control.MonadZero as MonadZero
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Time.Duration (class Duration, Minutes)
import Data.Time.Duration as Duration

newtype TimeZoneOffset = TimeZoneOffset Int

instance boundedTimeZoneOffset :: Bounded TimeZoneOffset where
  bottom = TimeZoneOffset (negate (23 * 60 + 59))
  top = TimeZoneOffset (23 * 60 + 59)

derive instance eqTimeZoneOffset :: Eq TimeZoneOffset

derive instance ordTimeZoneOffset :: Ord TimeZoneOffset

instance showTimeZoneOffset :: Show TimeZoneOffset where
  show (TimeZoneOffset min) = "(TimeZoneOffset " <> show min <> ")"

fromDuration :: forall a. Duration a => a -> Maybe TimeZoneOffset
fromDuration d = do
  let ms = Duration.fromDuration d
  MonadZero.guard (between (toDuration bottom) (toDuration top) ms)
  let
    (Duration.Minutes minutesNumber) =
      Duration.convertDuration ms :: Minutes
  minutesInt <- Int.fromNumber minutesNumber
  pure (TimeZoneOffset minutesInt)

toDuration :: forall a. Duration a => TimeZoneOffset -> a
toDuration (TimeZoneOffset offset) =
  Duration.convertDuration (Duration.Minutes (Int.toNumber offset))

utc :: TimeZoneOffset
utc = TimeZoneOffset 0
