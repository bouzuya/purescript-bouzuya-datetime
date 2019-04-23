module Test.Bouzuya.DateTime.OffsetDateTime
  ( tests
  ) where

import Prelude

import Bouzuya.DateTime.OffsetDateTime as OffsetDateTime
import Bouzuya.DateTime.TimeZoneOffset as TimeZoneOffset
import Data.Either as Either
import Data.Formatter.Parser.Interval as ParserInterval
import Data.Maybe as Maybe
import Data.String as String
import Data.Time.Duration as TimeDuration
import Partial.Unsafe as Unsafe
import Test.Unit (TestSuite)
import Test.Unit as TestUnit
import Test.Unit.Assert as Assert
import Text.Parsing.Parser as Parser

tests :: TestSuite
tests = TestUnit.suite "Bouzuya.DateTime.OffsetDateTime" do
  let
    jpOffset =
      Unsafe.unsafePartial
        (Maybe.fromJust
          (TimeZoneOffset.fromDuration (TimeDuration.Hours (-9.0))))
    utcOffset = TimeZoneOffset.utc
    dateTime1 =
      Unsafe.unsafePartial
        (Either.fromRight
          (Parser.runParser
            "2000-01-02T15:16:17Z"
            ParserInterval.parseDateTime))
    dateTime2 =
      Unsafe.unsafePartial
        (Either.fromRight
          (Parser.runParser
            "2000-01-02T06:16:17Z"
            ParserInterval.parseDateTime))
    -- 2000-01-03T00:16:17+09:00 == 2000-01-02T15:16:17Z (dateTime1)
    jpOffsetDateTime1 =
      Unsafe.unsafePartial
        (Maybe.fromJust (OffsetDateTime.fromUTCDateTime jpOffset dateTime1))
    -- 2000-01-02T15:16:17+09:00 == 2000-01-02T06:16:17Z (dateTime2)
    jpOffsetDateTime2 =
      Unsafe.unsafePartial
        (Maybe.fromJust (OffsetDateTime.fromUTCDateTime jpOffset dateTime2))
    -- 2000-01-03T00:16:17+09:00 == 2000-01-02T15:16:17Z (dateTime1)
    jpOffsetDateTimeString1 =
      String.joinWith
        " "
        [ "(OffsetDateTime"
        , String.joinWith
            " "
            [ "(UTCDateTime"
            , "(DateTime"
            , "(Date (Year 2000) January (Day 2))"
            , "(Time (Hour 15) (Minute 16) (Second 17) (Millisecond 0))))"
            ]
        , "(TimeZoneOffset (Minutes -540.0)))"
        ]
    -- 2000-01-02T15:16:17+09:00 == 2000-01-02T06:16:17Z (dateTime2)
    jpOffsetDateTimeString2 =
      String.joinWith
        " "
        [ "(OffsetDateTime"
        , String.joinWith
            " "
            [ "(UTCDateTime"
            , "(DateTime"
            , "(Date (Year 2000) January (Day 2))"
            , "(Time (Hour 6) (Minute 16) (Second 17) (Millisecond 0))))"
            ]
        , "(TimeZoneOffset (Minutes -540.0)))"
        ]
    -- 2000-01-02T15:16:17Z
    utcOffsetDateTimeString1 =
      String.joinWith
        " "
        [ "(OffsetDateTime"
        , String.joinWith
            " "
            [ "(UTCDateTime"
            , "(DateTime"
            , "(Date (Year 2000) January (Day 2))"
            , "(Time (Hour 15) (Minute 16) (Second 17) (Millisecond 0))))"
            ]
        , "(TimeZoneOffset (Minutes 0.0)))"
        ]

  TestUnit.test "Eq OffsetDateTime" do
    Assert.equal jpOffsetDateTime1 jpOffsetDateTime1

  TestUnit.test "Show OffsetDateTime" do
    Assert.equal jpOffsetDateTimeString1 (show jpOffsetDateTime1)
    Assert.equal utcOffsetDateTimeString1
      (show (OffsetDateTime.inUTC jpOffsetDateTime1))

  TestUnit.test "fromLocalDateTime" do
    Assert.equal
      (pure jpOffsetDateTimeString2)
      (map show (OffsetDateTime.fromLocalDateTime jpOffset dateTime1))
    Assert.equal
      (pure jpOffsetDateTimeString1)
      (map
        show
        (bind
          (OffsetDateTime.fromLocalDateTime utcOffset dateTime1)
          (OffsetDateTime.inOffset jpOffset)))

  TestUnit.test "fromUTCDateTime" do
    Assert.equal
      (pure jpOffsetDateTimeString1)
      (map show (OffsetDateTime.fromUTCDateTime jpOffset dateTime1))

  TestUnit.test "fromUTCDateTimeInUTC" do
    Assert.equal
      utcOffsetDateTimeString1
      (show (OffsetDateTime.fromUTCDateTimeInUTC dateTime1))
    Assert.equal
      (pure jpOffsetDateTimeString1)
      (map
        show
        (OffsetDateTime.inOffset
          jpOffset
          (OffsetDateTime.fromUTCDateTimeInUTC dateTime1)))

  TestUnit.test "inOffset" do
    Assert.equal
      (pure jpOffsetDateTimeString1)
      (map
        show
        (OffsetDateTime.inOffset
          jpOffset
          (OffsetDateTime.fromUTCDateTimeInUTC dateTime1)))
    Assert.equal
      (pure utcOffsetDateTimeString1)
      (map
        show
        (OffsetDateTime.inOffset
          utcOffset
          (OffsetDateTime.fromUTCDateTimeInUTC dateTime1)))

  TestUnit.test "inUTC" do
    Assert.equal
      utcOffsetDateTimeString1
      (show (OffsetDateTime.inUTC jpOffsetDateTime1))
    Assert.equal
      utcOffsetDateTimeString1
      (show
        (OffsetDateTime.inUTC
          (OffsetDateTime.fromUTCDateTimeInUTC dateTime1)))

  TestUnit.test "timeZoneOffset" do
    Assert.equal jpOffset (OffsetDateTime.timeZoneOffset jpOffsetDateTime1)
    Assert.equal
      utcOffset
      (OffsetDateTime.timeZoneOffset
        (OffsetDateTime.inUTC jpOffsetDateTime1))

  TestUnit.test "toLocalDateTime" do
    Assert.equal
      dateTime1
      (OffsetDateTime.toLocalDateTime jpOffsetDateTime2)

  TestUnit.test "toUTCDateTime" do
    Assert.equal
      dateTime1
      (OffsetDateTime.toUTCDateTime jpOffsetDateTime1)
    Assert.equal
      dateTime2
      (OffsetDateTime.toUTCDateTime jpOffsetDateTime2)
