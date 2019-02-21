module Bouzuya.Date.Extra
  ( firstDateOfYear
  , lastDateOfYear
  ) where

import Data.Date (Date, Year)
import Data.Date as Date
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Prelude (bottom, top)

firstDateOfYear :: Year -> Date
firstDateOfYear y =
  Unsafe.unsafePartial (Maybe.fromJust (Date.exactDate y bottom bottom))

lastDateOfYear :: Year -> Date
lastDateOfYear y =
  Unsafe.unsafePartial (Maybe.fromJust (Date.exactDate y top top))
