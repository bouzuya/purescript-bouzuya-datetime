module Test.Bouzuya.DateTime.TimeZoneOffset
  ( tests
  ) where

import Bouzuya.DateTime.TimeZoneOffset (TimeZoneOffset)
import Bouzuya.DateTime.TimeZoneOffset as TimeZoneOffset
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Hours(..), Milliseconds, Minutes(..), Seconds(..))
import Data.Time.Duration as Duration
import Prelude (bottom, discard, map, negate, show, top, (*), (+), (<))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "TimeZoneOffset" do
  test "Bounded TimeZoneOffset" do
    Assert.equal
      (TimeZoneOffset.fromDuration (Minutes (negate (23.0 * 60.0 + 59.0))))
      (Just (bottom :: TimeZoneOffset))
    Assert.equal
      (TimeZoneOffset.fromDuration (Minutes (23.0 * 60.0 + 59.0)))
      (Just (top :: TimeZoneOffset))

  test "Eq TimeZoneOffset" do
    Assert.equal TimeZoneOffset.utc TimeZoneOffset.utc

  test "Ord TimeZoneOffset" do
    Assert.assert
      "-09:00 < +09:00"
      ((TimeZoneOffset.fromDuration (Minutes (-540.0))) <
        (TimeZoneOffset.fromDuration (Minutes (540.0))))

  test "Show TimeZoneOffset" do
    Assert.equal
      (Just "(TimeZoneOffset 540)")
      (map show (TimeZoneOffset.fromDuration (Minutes (540.0))))

  test "fromDuration / toDuration" do
    Assert.equal
      (Just TimeZoneOffset.utc)
      (TimeZoneOffset.fromDuration (Hours 0.0))
    Assert.equal
      (Just "(TimeZoneOffset 540)")
      (map show (TimeZoneOffset.fromDuration (Minutes 540.0)))
    for_
      [ Duration.fromDuration (Hours 0.0)
      , Duration.fromDuration (Hours 9.0)
      , Duration.fromDuration (Hours (negate 9.0))
      ]
      \d -> do
        Assert.equal
          (Just d)
          (map TimeZoneOffset.toDuration (TimeZoneOffset.fromDuration d))
    for_
      [ Duration.fromDuration (Hours (negate 24.0))
      , Duration.fromDuration (Hours 24.0)
      , Duration.fromDuration (Seconds 1.0)
      ]
      \d -> do
        Assert.equal
          (Nothing :: _ Milliseconds)
          (map TimeZoneOffset.toDuration (TimeZoneOffset.fromDuration d))

  test "utc" do
    Assert.equal "(TimeZoneOffset 0)" (show TimeZoneOffset.utc)
