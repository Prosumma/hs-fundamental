module Fundamental.Formatting (
    displayText,
    uformat,
) where

import Formatting
import RIO

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Builder as T

displayText :: Text -> Utf8Builder
displayText = display

uformat :: Format Utf8Builder a -> a
uformat m = runFormat m (display . T.toStrict . T.toLazyText)
