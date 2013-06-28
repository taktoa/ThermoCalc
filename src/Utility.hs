-- Collection of utility functions
module Utility where

import Data.Function (on)
import Data.List (minimumBy)
import Data.Decimal
import Data.Word

e a b = a * (10.0 ** b)                                     -- "e" function; i.e.: 1.3e6 = 1.3 * (10^6) = 1300000

sin2 x = sin x ** 2.0                                       -- Trigonometric utility functions
cos2 x = cos x ** 2.0
tan2 x = tan x ** 2.0
csc x = 1.0 / sin x
sec x = 1.0 / cos x
cot x = 1.0 / tan x
csc2 x = csc x ** 2.0
sec2 x = sec x ** 2.0
cot2 x = cot x ** 2.0

places :: Int                                               -- Max number of decimal places in a printed number
places = 10

show' :: Double -> String                                   -- Prints fixed-length numbers
show' x
    | al < places       = a ++ replicate (places - al) '0'
    | al > places       = take places a
    | al == places      = a
    where
    a = show (realFracToDecimal (fromIntegral places :: Word8) x)
    al = length a

acc = 0.01                                                  -- Accuracy of the root-finder

listgen (m,n) d = [m, (m + d) .. n]                         -- Naive root finder function

bestRoot f brckt d = xs !! index
    where
    xs = listgen brckt d
    index = fst (minimumBy (compare `on` snd) zipped)
    zipped = zip [0 .. length ys] ys
    ys = map f xs
