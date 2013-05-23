-- Collection of utility functions
module Utility where

import Data.Decimal
import Data.Word

e a b = a * (10.0 ** b)

sin2 x = sin x ** 2.0
cos2 x = cos x ** 2.0
cot x = 1.0 / tan x

places :: Int
places = 12

show' :: Double -> String
show' x
    | al < places       = a ++ replicate (places - al) '0'
    | al > places       = take places a
    | al == places      = a
    where
    a = show (realFracToDecimal (fromIntegral places :: Word8) x)
    al = length a
