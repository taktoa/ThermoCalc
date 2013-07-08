-- Collection of utility functions
module Utility where

import Numeric.Units.Dimensional.Prelude (ThermodynamicTemperature, ElectricResistance, Dim, Quantity, (^), pos2)
import Numeric.NumType (Pos1, Neg1, Pos2, Neg2, Zero)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Decimal
import Data.Word

a // b = (fromIntegral a) / (fromIntegral b)
e a b = a * (10.0 ** b)                                     -- "e" function; i.e.: 1.3e6 = 1.3 * (10^6) = 1300000

pyth a b = sqrt ((a ** 2) + (b ** 2))

sin2 x = sin x ** 2.0                                       -- Trigonometric utility functions
cos2 x = cos x ** 2.0
tan2 x = tan x ** 2.0
csc x = 1.0 / sin x
sec x = 1.0 / cos x
cot x = 1.0 / tan x
csc2 x = csc x ** 2.0
sec2 x = sec x ** 2.0
cot2 x = cot x ** 2.0

fst3 (a, _, _) = a
snd3 (_, b, _) = b
thd3 (_, _, c) = c

fst4 (a, _, _, _) = a
snd4 (_, b, _, _) = b
thd4 (_, _, c, _) = c
lst4 (_, _, _, d) = d

log10 = logBase 10

type Temperature = ThermodynamicTemperature
type Resistance = ElectricResistance
type DCompliance = Dim Zero Neg1 Pos2 Zero Zero Zero Zero
type Compliance = Quantity DCompliance
type DSpringConstant = Dim Zero Pos1 Neg2 Zero Zero Zero Zero
type SpringConstant = Quantity DSpringConstant
type DBLValue = Dim Pos1 Pos1 Neg2 Neg1 Zero Zero Zero
type BLValue = Quantity DBLValue
a !+ b = a Prelude.+ b
a !* b = a Prelude.* b
a !/ b = a Prelude./ b
a !** b = a Prelude.** b

squ a = a Numeric.Units.Dimensional.Prelude.^ pos2

places :: Int                                               -- Max number of decimal places in a printed number
places = 8

round_ :: Int -> Double -> Double
round_ p x = round (x * (10 Prelude.^ p)) // (10 Prelude.^ p)

show_ :: Int -> Double -> String                            -- Prints fixed-length numbers
show_ p x
    | al < p            = a ++ replicate (places - al) '0'
    | al > p            = take places a
    | al == p           = a
    where
    a = show (realFracToDecimal (fromIntegral p :: Word8) x)
    al = length a
    
show' = show_ places

acc = 0.01                                                  -- Accuracy of the root-finder

listgen (m,n) d = [m, (m + d) .. n]                         -- Naive root finder function

bestRoot f brckt d = xs !! index
    where
    xs = listgen brckt d
    index = fst (minimumBy (compare `on` snd) zipped)
    zipped = zip [0 .. length ys] ys
    ys = map f xs

applyError f x inerr = (x, lower, y, upper)
    where
    y = f x
    a = f (x * (1 + inerr))
    b = f (x * (1 - inerr))
    upper = if a > b then a else b
    lower = if a > b then b else a
    
rootError f brckt d = applyError f root
    where
    root = bestRoot f brckt d

circleArea r = pi * r**2.0
circleArea' d = (1/4) * pi * d**2.0

sphereVol r = (4/3) * pi * r**3.0
sphereVol' d = (1/6) * pi * d**3.0
