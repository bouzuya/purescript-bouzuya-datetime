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
  bottom = TimeZoneOffset (negate (24 * 60 * 60 - 1))
  top = TimeZoneOffset (24 * 60 * 60 - 1)

derive instance eqTimeZoneOffset :: Eq TimeZoneOffset

derive instance ordTimeZoneOffset :: Ord TimeZoneOffset

instance showTimeZoneOffset :: Show TimeZoneOffset where
  show offset =
    let
      minutes :: Minutes
      minutes = toDuration offset
    in "(TimeZoneOffset " <> show minutes <> ")"

fromDuration :: forall a. Duration a => a -> Maybe TimeZoneOffset
fromDuration d = do
  let ms = Duration.fromDuration d
  MonadZero.guard (between (toDuration bottom) (toDuration top) ms)
  let (Duration.Seconds secondsNumber) = Duration.convertDuration ms
  secondsInt <- Int.fromNumber secondsNumber
  pure (TimeZoneOffset secondsInt)

toDuration :: forall a. Duration a => TimeZoneOffset -> a
toDuration (TimeZoneOffset offset) =
  Duration.convertDuration (Duration.Seconds (Int.toNumber offset))

utc :: TimeZoneOffset
utc = TimeZoneOffset 0
