-- Collection of utility functions
module Utility where

import Numeric.Units.Dimensional.Prelude (ThermodynamicTemperature, ElectricResistance, Dim, Dimensionless, Quantity, pos2, _5, _2, (/~~), (/~), (*~), one)
import qualified Numeric.Units.Dimensional.Prelude ((^), (+), (**), (-), (*), (/), log)
import qualified Numeric.Units.Dimensional
import Numeric.NumType (Pos1, Neg1, Pos2, Neg2, Neg3, Zero)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Decimal
import Data.Word

a // b = fromIntegral a / fromIntegral b
e a b = a * (10.0 ** b)                                     -- "e" function; i.e.: 1.3e6 = 1.3 * (10^6) = 1300000
e' a b = a !* (10.0 !** b)                                  -- "e" function; i.e.: 1.3e6 = 1.3 * (10^6) = 1300000

cang = 9.0                                                  -- Half-angle of the cones, in degrees

pyth a b = sqrt ((a ** 2) + (b ** 2))

dtr x = pi * x / 180.0                                      -- Degrees to radians conversion
rtd x = 180.0 * x / pi                                      -- Radians to degrees conversion

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
log10' :: (Floating a) => Dimensionless a -> Dimensionless a
log10' = fmap (logBase 10)

isNaN' :: (RealFloat a) => Dimensionless a -> Bool
isNaN' x = isNaN (x /~ one)

type Temperature = ThermodynamicTemperature
type Resistance = ElectricResistance
type DimlessDouble = Dimensionless Double
type DCompliance = Dim Zero Neg1 Pos2 Zero Zero Zero Zero
type Compliance = Quantity DCompliance
type DSpringConstant = Dim Zero Pos1 Neg2 Zero Zero Zero Zero
type SpringConstant = Quantity DSpringConstant
type DBLValue = Dim Pos1 Pos1 Neg2 Neg1 Zero Zero Zero
type BLValue = Quantity DBLValue
type DVoltage = Dim Pos2 Pos1 Neg3 Neg1 Zero Zero Zero
type Voltage = Quantity DVoltage

imap :: [a -> b] -> a -> [b]
imap fs s = zipWith ($) fs (replicate (length fs) s)

a !+ b = a Prelude.+ b
a !- b = a Prelude.- b
a !* b = a Prelude.* b
a !/ b = a Prelude./ b
a !^ b = a Prelude.^ b
a !** b = a Prelude.** b
psqrt = Prelude.sqrt
pexp = Prelude.exp
ppyth a b = psqrt ((a !** 2.0) !+ (b !** 2.0))
a !^/ b = b Prelude.** (1 Prelude./ a)

a #+ b = a Numeric.Units.Dimensional.Prelude.+ b
a #* b = a Numeric.Units.Dimensional.Prelude.* b
a #/ b = a Numeric.Units.Dimensional.Prelude./ b
a #^ b = a Numeric.Units.Dimensional.Prelude.^ b
a #** b = a Numeric.Units.Dimensional.Prelude.** b

funcMap :: [a -> b] -> a -> [b]
funcMap fs x = zipWith ($) fs xs
        where
        xs = replicate (length fs) x

squ a = a #^ pos2

places :: Int                                               -- Max number of decimal places in a printed number
places = 8

round_ :: (Floating a, RealFrac a) => Int -> a -> Decimal
round_ p x = realFracToDecimal (fromIntegral p::Word8) (round (x * (10 !^ p)) // (10 !^ p))

round' :: (Floating a, RealFrac a) => Int -> Quantity d a -> Quantity d Decimal
round' p (Numeric.Units.Dimensional.Dimensional x) = Numeric.Units.Dimensional.Dimensional (round_ p x)

show_ :: Int -> Double -> String                            -- Prints fixed-length numbers
show_ p x
    | al < p            = a ++ replicate (places - al) '0'
    | al > p            = take places a
    | al == p           = a
    where
    a = show (realFracToDecimal (fromIntegral p :: Word8) x)
    al = length a
    
acc = 0.01
accD = acc *~ one

listgen :: (Enum a, Num a) => (a, a) -> a -> [a]
listgen (m,n) d = [m, (m !+ d) .. n]

listgen' :: (DimlessDouble, DimlessDouble) -> DimlessDouble -> [DimlessDouble]
listgen' (m,n) d = [m, (m #+ d) .. n]

bestRoot f brckt d = xs !! index
    where
    xs = listgen brckt d
    index = fst (minimumBy (compare `on` snd) zipped)
    zipped = zip [0 .. length ys] ys
    ys = map f xs

bestRoot' :: (DimlessDouble -> DimlessDouble) -> (DimlessDouble, DimlessDouble) -> DimlessDouble -> DimlessDouble
bestRoot' f brckt d = xs !! index
    where
    xs = listgen' brckt d
    index = fst (minimumBy (compare `on` snd) zipped)
    zipped = zip [0 .. length ys] ys
    ys = map f xs /~~ one

circleArea r = pi * r**2.0
circleArea' d = (1/4) * pi * d**2.0

sphereVol r = (4/3) * pi * r**3.0
sphereVol' d = (1/6) * pi * d**3.0
