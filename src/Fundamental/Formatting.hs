module Fundamental.Formatting
  ( displayText,
    uformat,
  )
where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T
import Formatting
import RIO

displayText :: Text -> Utf8Builder
displayText = display

uformat :: Format Utf8Builder a -> a
uformat m = runFormat m (display . T.toStrict . T.toLazyText)
