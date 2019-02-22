module Test.Bouzuya.Date.YearMonth
  ( tests
  ) where

import Bouzuya.Date.YearMonth as YearMonth
import Data.Date as Date
import Data.Enum as Enum
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (bind, discard, pure, (<$>), (<*>), (>>=))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "Bouzuya.Date.YearMonth" do
  test "fromDate / toDate" do
    let
      d1Maybe = do
        y <- Enum.toEnum 2000
        m <- Enum.toEnum 1
        d <- Enum.toEnum 2
        Date.exactDate y m d
      d1 = Unsafe.unsafePartial (Maybe.fromJust d1Maybe)
    Assert.equal
      (Just d1)
      (YearMonth.toDate (Date.day d1) (YearMonth.fromDate d1))

  test "yearMonth" do
    y <- pure (Enum.toEnum 2000)
    m <- pure (Enum.toEnum 1)
    d <- pure (Enum.toEnum 2)
    dateMaybe <-
      pure do
        y' <- y
        m' <- m
        d' <- d
        Date.exactDate y' m' d'
    date <- pure (Unsafe.unsafePartial (Maybe.fromJust dateMaybe))
    ym <- pure (YearMonth.yearMonth <$> y <*> m)
    -- TODO: Eq YearMonth
    Assert.equal (Just date) (ym >>= (YearMonth.toDate (Date.day date)))
