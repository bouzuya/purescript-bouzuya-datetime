module Test.Bouzuya.DateTime.WeekDate.Extra
  ( tests
  ) where

import Bouzuya.DateTime.WeekDate.Extra as WeekDateExtra
import Data.Date as Date
import Data.Enum as Enum
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Prelude (bind, bottom, discard, top, (<$>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.DateTime.WeekDate.Extra" do
  test "firstWeekdayOfYear" do
    let
      date y = do
        y' <- Enum.toEnum y
        Date.exactDate y' bottom bottom
      cases =
        [ Tuple Date.Monday 2018
        , Tuple Date.Tuesday 2019
        , Tuple Date.Wednesday 2014
        , Tuple Date.Thursday 2015
        , Tuple Date.Friday 2016
        , Tuple Date.Saturday 2011
        , Tuple Date.Sunday 2017
        ]
    Foldable.for_ cases \(Tuple wday y) -> do
      Assert.equal (Just wday) (Date.weekday <$> (date y))
      Assert.equal
        (Just wday)
        (WeekDateExtra.firstWeekdayOfYear <$> (Enum.toEnum y))

  test "lastWeekdayOfYear" do
    let
      date y = do
        y' <- Enum.toEnum y
        Date.exactDate y' top top
      cases =
        [ Tuple Date.Monday 2018
        , Tuple Date.Tuesday 2019
        , Tuple Date.Wednesday 2014
        , Tuple Date.Thursday 2015
        , Tuple Date.Friday 2010
        , Tuple Date.Saturday 2016
        , Tuple Date.Sunday 2017
        ]
    Foldable.for_ cases \(Tuple wday y) -> do
      Assert.equal (Just wday) (Date.weekday <$> (date y))
      Assert.equal
        (Just wday)
        (WeekDateExtra.lastWeekdayOfYear <$> (Enum.toEnum y))
