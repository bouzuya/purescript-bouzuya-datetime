module Test.Bouzuya.DateTime.TimeZoneOffset
  ( tests
  ) where

import Prelude

import Bouzuya.DateTime.TimeZoneOffset (TimeZoneOffset)
import Bouzuya.DateTime.TimeZoneOffset as TimeZoneOffset
import Data.Foldable as Foldable
import Data.Int as Int
import Data.Maybe as Maybe
import Data.Newtype as Newtype
import Data.Time.Duration (Milliseconds)
import Data.Time.Duration as Duration
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = TestUnit.suite "TimeZoneOffset" do
  TestUnit.test "Bounded TimeZoneOffset" do
    Assert.equal
      (TimeZoneOffset.fromDuration
        (Duration.Seconds (negate (Int.toNumber (24 * 60 * 60 - 1)))))
      (Maybe.Just (bottom :: TimeZoneOffset))
    Assert.equal
      (TimeZoneOffset.fromDuration
        (Duration.Seconds (Int.toNumber (24 * 60 * 60 - 1))))
      (Maybe.Just (top :: TimeZoneOffset))

  TestUnit.test "Eq TimeZoneOffset" do
    Assert.equal TimeZoneOffset.utc TimeZoneOffset.utc

  TestUnit.test "Ord TimeZoneOffset" do
    Assert.assert
      "+09:00 < -09:00"
      ((TimeZoneOffset.fromDuration (Duration.Minutes (-540.0))) <
        (TimeZoneOffset.fromDuration (Duration.Minutes (540.0))))

  TestUnit.test "Show TimeZoneOffset" do
    Assert.equal
      (Maybe.Just "(TimeZoneOffset (Minutes 540.0))") -- -09:00
      (map show (TimeZoneOffset.fromDuration (Duration.Minutes (540.0))))
    Assert.equal
      (Maybe.Just "(TimeZoneOffset (Minutes -540.0))") -- +09:00
      (map show (TimeZoneOffset.fromDuration (Duration.Minutes (-540.0))))

  TestUnit.test "fromDuration / toDuration" do
    Assert.equal
      (Maybe.Just TimeZoneOffset.utc)
      (TimeZoneOffset.fromDuration (Duration.Hours 0.0))
    Assert.equal
      (Maybe.Just "(TimeZoneOffset (Minutes 540.0))")
      (map show (TimeZoneOffset.fromDuration (Duration.Minutes 540.0)))
    Foldable.for_
      [ Duration.fromDuration (Duration.Hours 0.0)
      , Duration.fromDuration (Duration.Hours 9.0)
      , Duration.fromDuration (Duration.Hours (-9.0))
      , Duration.fromDuration (Duration.Seconds 1.0) -- OK
      , Newtype.over2 Duration.Milliseconds (+)
          (Duration.fromDuration (Duration.Minutes 25.0))
          (Duration.fromDuration (Duration.Seconds 21.0)) -- UTC−00:25:21
      ]
      \d -> do
        Assert.equal
          (Maybe.Just d)
          (map TimeZoneOffset.toDuration (TimeZoneOffset.fromDuration d))
    Foldable.for_
      [ Duration.fromDuration (Duration.Hours (negate 24.0))
      , Duration.fromDuration (Duration.Hours 24.0)
      ]
      \d -> do
        Assert.equal
          (Maybe.Nothing :: _ Milliseconds)
          (map TimeZoneOffset.toDuration (TimeZoneOffset.fromDuration d))

  TestUnit.test "utc" do
    Assert.equal "(TimeZoneOffset (Minutes 0.0))" (show TimeZoneOffset.utc)
