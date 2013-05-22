module Main where

import Graphics.EasyPlot
import Math.Root.Finder
import Data.Function (on)
import Data.List (minimumBy)
import Data.Decimal
import Data.Word

-- Useful functions
e a b = a * (10.0 ** b)
sin2 x = (sin x)**2.0
cos2 x = (cos x)**2.0
cot x = 1.0 / (tan x)
places :: Int
places = 12
show' :: Double -> String
show' x
    | al < places       = a ++ (replicate (places - al) '0')
    | al > places       = take places a
    | al == places      = a
    where
    a = show (realFracToDecimal ((fromIntegral places)::Word8) x)
    al = length a
--show' = show


-- Input variables
p = 10.0                                -- bar
t = 298.15                              -- kelvin
dt = 10.0                               -- kelvin
cd = 0.62                               -- sq cells/mm^2
-- br = 0.83, hd = 1.17 mm
-- Input dimensions
lt = 1000.0                             -- millimeter
d1 = 83.41                              -- millimeter
d2 = 44.58                              -- millimeter
y0 = 1.17/4                             -- millimeter
st = 0.06                               -- millimeter

-- Speaker Properties
spPrms = 25                             -- watt
spPmax = 50                             -- watt
spfmin = 80                             -- hertz
spfmax = 6000                           -- hertz
spfres = 77                             -- hertz
sps = 86                                -- dB 1W / 1m
sprdc = 5.6                             -- ohm
spinduc = 0.60                          -- millihenry
spqms = 2.43                            -- dimensionless
spqes = 0.90                            -- dimensionless
spqts = 0.66                            -- dimensionless
spvas = 5.1                             -- liter
spxmax = 1.5                            -- millimeter
splen = 60                              -- millimeter
spdsmall = 115                          -- millimeter
spdscrew = 127                          -- millimeter
spdtotal = 146                          -- millimeter

-- Derived dimensions
br = y0 / (y0 + st)                     -- dimensionless
dr = d1/d2                              -- dimensionless
r1 = d1/2                               -- millimeter
r2 = d2/2                               -- millimeter
lsph = d1/2                             -- millimeter
u = 100000000*dp/(rho*sos)              -- mm/s
lhex = (u/omg)*sin(k*lrest)             -- millimeter
lc = (d1 - d2) / (2.0 * (tan 0.15708))  -- millimeter
wl = 4.0 * lt                           -- millimeter
xa1 = pi * (r1 ** 2.0)                  -- square millimeter
xa2 = pi * (r2 ** 2.0)                  -- square millimeter

vc = pi*lb*((r1**2)+(r2**2)+(r1*r2))/3  -- mm^3
vb = xa2*lb                             -- mm^3
vsph = (4/3)*pi*(lsph**3)               -- mm^3
vhex = xa1*lhex                         -- mm^3
lrest = lt - ((2*lc)+lb+lsph)           -- millimeters
vrest = xa1*lrest                       -- mm^3
vresti = xa1*(lrest+spxmax)             -- mm^3
vrestf = xa1*(lrest-spxmax)             -- mm^3
vtotal = vrest + (2*vc) + vsph + vb     -- mm^3
vtotali = vresti + (2*vc) + vsph + vb   -- mm^3
vtotalf = vrestf + (2*vc) + vsph + vb   -- mm^3

-- Constants
cp = 5.193                              -- J / (g*K)
cv = 3.116                              -- J / (g*K)
gam = cp/cv                             -- dimensionless

-- Regressions
sos = 1000.0 * (480.589 + (1.7875 * t)) -- mm/s
kg = ((2.38889 `e` (-4)) * t) ** 0.710  -- W / (m*K)
mu = ((7.96389 `e` (-6)) * t) ** 0.647  -- centipoise
rho = ((47.90903 * p) + 0.1472) / t     -- g/mL
pr = (mu*cp)/kg                         -- dimensionless

-- Derived values
mach = (100000000*dp)/(rho*(sos**2))    -- Mach
f = sos / wl                            -- hertz
omg = 2*pi*f                            -- rps
k = (2*pi) / wl                         -- mm^-1
dp = dpn*p                              -- bar
dtn = dt / t                            -- dimensionless
dpn = (vtotali-vtotalf)/vtotal          -- dimensionless
dk = sqrt (kg / (rho*cp*pi*f))          -- millimeters
dkn = dk / y0                           -- dimensionless
dv = sqrt (mu / (rho*pi*f))             -- millimeters
dvn = dv / y0                           -- dimensionless

-- Constraints
tempL = 1 - (dkn * sqrt pr) + (0.5 * pr * (dkn**2))
tempA x = (dkn * (dpn**2) * (sin x)) / (8 * gam * (1 + pr) * tempL)
tempB x l = (dtn * tan x) / ((gam - 1) * br * l)
tempC = (1 + pr + sqrt pr) / (1 + (sqrt pr))
tempD = 1 - (dkn * sqrt pr) + sqrt pr
qcn x l = (tempA x) * ((tempC * tempB x l) - tempD)

tempE x l = (dkn * l * (dpn**2) * (gam - 1) * br * cos2 x) / (4 * gam)
tempF x l = (dtn * tan x) / (br * l * (gam - 1) * (1 + (sqrt pr)) * tempL)
tempG x l = (dkn * l * (dpn**2) * (sqrt pr) * (sin2 x)) / (4 * gam * br * tempL)
wn x l = ((tempE x l) * ((tempF x l) - 1)) - (tempG x l)

cop xn ln = (qcn xn ln) / (wn xn ln)

---

tempH lb = cot (k*(lt - (lsph + (2*lc) + lb)))
tempI lb = (dr**2) * tan (k*(lb + lc))
optHI lb = abs ((tempH lb) - (tempI lb))
acc = 0.01
lmax = lt - ((2*lc) + lsph)
brckt = ((lmax/2), lmax)
findRoots f start stop step = [x | x <- [start, start+step .. stop], abs (f x) < step]
rootXs = uncurry (findRoots optHI) brckt acc
rootYs = map optHI rootXs
zippedrootYs = zip [0 .. length rootYs] rootYs
index = fst (minimumBy (compare `on` snd) zippedrootYs)
lb = lmax - (rootXs !! index)

-- Checks
small = 0.05
maxMach = 0.1
checkMach = mach < maxMach
--checkStack = Ls < (small/k)
checkTPD = 2*y0*small > dk
checkVPD = 2*y0*small > dv
checkTD = t*small > dt
checkStack = (y0 > (2*dk)) && (y0 < (4*dk))

-------------------------------------------------------------------

enviroPrint = do
    putStrLn "------------------"
    putStrLn "ENVIRONMENT:"
    putStrLn ("Pressure:            " ++ show' p        ++ " bar")
    putStrLn ("Temp:                " ++ show' t        ++ " K")
    putStrLn ""

gaspropPrint = do
    putStrLn "------------------"
    putStrLn "GAS PROPERTIES:"
    putStrLn ("C_p:                 " ++ show' cp       ++ " J/(g*K)")
    putStrLn ("C_v:                 " ++ show' cv       ++ " J/(g*K)")
    putStrLn ("Gamma:               " ++ show' gam      ++ "")
    putStrLn ("Prandtl Number:      " ++ show' pr       ++ "")
    putStrLn ("SoS:                 " ++ show' sos      ++ " mm/s")
    putStrLn ("Viscosity:           " ++ show' mu       ++ " cP")
    putStrLn ("Conductivity:        " ++ show' kg       ++ " W/(m*K)")
    putStrLn ("Density:             " ++ show' rho      ++ " g/mL")
    putStrLn ""

dimensionsPrint = do
    putStrLn "------------------"
    putStrLn "DIMENSIONS:"    
    putStrLn ("Diameter #1:         " ++ show' d1       ++ " mm")
    putStrLn ("Diameter #2:         " ++ show' d2       ++ " mm")
    putStrLn ("Total length:        " ++ show' lt       ++ " mm")
    putStrLn ("Thin tube length:    " ++ show' lb       ++ " mm")
    putStrLn ("Cap length:          " ++ show' lsph     ++ " mm")
    putStrLn ("HEX length:          " ++ show' lhex     ++ " mm")
    putStrLn ("Cone length:         " ++ show' lc       ++ " mm")
    putStrLn ("Stack spacing:       " ++ show' (y0/2)   ++ " mm")
    putStrLn ("X-section area #1:   " ++ show' xa1      ++ " mm^2")
    putStrLn ("X-section area #2:   " ++ show' xa2      ++ " mm^2")
    putStrLn ("Block ratio:         " ++ show' br       ++ "")
    putStrLn ("Radius ratio:        " ++ show' dr       ++ "")
    putStrLn ""

syspropPrint = do
    putStrLn "------------------"
    putStrLn "SYSTEM PROPERTIES:"
    putStrLn ("Mach #:              " ++ show' mach     ++ " Mach")
    putStrLn ("Frequency:           " ++ show' f        ++ " Hz")
    putStrLn ("Wavelength:          " ++ show' wl       ++ " mm")
    putStrLn ("Normalizer:          " ++ show' k        ++ " mm^-1")
    putStrLn ("Temp. diff.:         " ++ show' dt       ++ " K")
    putStrLn ("Norm. temp. diff.:   " ++ show' dtn      ++ "")
    putStrLn ("Pres. diff.:         " ++ show' dp       ++ " bar")
    putStrLn ("Norm. pres. diff.:   " ++ show' dpn      ++ "")
    putStrLn ("Thermal PD:          " ++ show' dk       ++ " mm")
    putStrLn ("Normalized TPD:      " ++ show' dkn      ++ "")
    putStrLn ("Viscous PD:          " ++ show' dv       ++ " mm")
    putStrLn ("Normalized VPD:      " ++ show' dvn      ++ "")
    putStrLn ""

diagChecks = do
    putStrLn "------------------"
    putStrLn "DIAGNOSTICS:"
    putStrLn (if checkMach  then "Mach number check passed."    else "Mach number too high!")
    putStrLn (if checkTPD   then "TPD check passed."            else "TPD check failed!")
    putStrLn (if checkVPD   then "VPD check passed."            else "VPD check failed!")
    putStrLn (if checkTD    then "TD check passed."             else "TD check failed!")
    putStrLn (if checkStack then "Stack check passed."          else "Stack check failed!")
    putStrLn ""

diagnostic = do
    enviroPrint
    gaspropPrint
    dimensionsPrint
    syspropPrint
    diagChecks
--    let options = [Title "COP vs x and L"]
--    let options3D = [RangeX 0 0.5, RangeY 0 1, StepX (acc*2), StepY (acc*2)]
    --let options3D = [RangeX 0 0.3, RangeY 0 0.3, StepX acc, StepY acc]
--    let big = 10
--    let nonan x l
--            | isNaN a       = big
--            | a > big       = big
--            | a < -big      = -big
--            | otherwise     = a
--            where
--            a = (qcn x l) --a = cop x l
--    let func = Function3D options options3D nonan
--    plot' [Interactive] X11 $ func
    let options = [Title "COP vs L"]
    let options2D = [Range 0 1, Step (acc*2)]
    let big = 10
    let nonan l
            | isNaN a       = big
            | a > big       = big
            | a < -big      = -big
            | otherwise     = a
            where
            a = (cop 0.25 l)
    let func = Function2D options options2D nonan
    plot' [Interactive] X11 $ func

main = diagnostic


