module Helio.Color
  ( Color(..)
  , toRGB
  , fromHex
  )
where

import Helio.Common
import Data.Bits (shiftL, shiftR, (.&.))

toRGB :: Color -> Int
toRGB (Color red green blue) = (red `shiftL` 8 + green) `shiftL` 8 + blue

fromHex :: Int -> Color
fromHex rgb =
  let
    red = (rgb `shiftR` 16) .&. 0xFF
    green = (rgb `shiftR` 8) .&. 0xFF
    blue = rgb .&. 0xFF
  in 
    Color red green blue

