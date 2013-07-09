-- Derived values, calculated based on the input constants
module Derived where

import Input
import Utility (sin2, cos2, cot, log10, bestRoot, acc)

-- Derived dimensions
lr = 2 * (lrest - (xn / k))             -- mm               -- Regenerator length
lta = lrest - lr                        -- mm               -- Length of the initial tube

cAngRad = cang * (pi/180.0)             -- rad              -- Cone half-angle
lc = (d1 - d2) / (2.0 * tan cAngRad)    -- mm               -- Cone length

dr = d1/d2                              -- DL               -- Diameter ratio

r0 = d0/2                               -- mm               -- Radius of speaker tube
r1 = d1/2                               -- mm               -- Radius of initial tube
r2 = d2/2                               -- mm               -- Radius of smaller tube

lsph = (d1/2)                           -- mm               -- Semispherical endcap length
u = (10^8)*dp/(rho*sos)                 -- mm/s             -- Temporary variable
lhex = (dp/(rho*sos*omg)) * sin(k*lrest)  -- mm               -- Heat exchanger length

xa0 = circleArea r0                     -- mm^2             -- Speaker cabinet cross-sectional area
xa1 = circleArea r1                     -- mm^2             -- Cross-sectional area 1
xa2 = circleArea r2                     -- mm^2             -- Cross-sectional area 2
xa3 = circleArea' spdsmall              -- mm^2             -- Cross-sectional area of speaker


lrest = lt - ((2*lc)+lb+lsph)           -- mm               -- Length of the rest of the resonator

dvmax = spsd*spxmax                     -- mm^3             -- Change in volume as the speaker cycles

ltotal = lbox + lt                      -- mm               -- Total length of device

dprms = dpn / sqrt 2                    -- DL               -- Dynamic RMS pressure
loud = 20 * (log10 (dprms / 20) + 11)   -- dB SPL           -- Sound pressure in decibels relative to the interior sound threshold

-- Constraints
qcn x l i = tempA x * ((tempC * tempB x l) - tempD)
        where
        tempA x = (dkn * (dpn**2) * sin x) / (8 * gam * (1 + pr) * tempL)
        tempB x l = (dtn * tan x) / ((gam - 1) * br * l)
        tempC = (1 + pr + sqrt pr) / (1 + sqrt pr)
        tempD = 1 - (dkn * sqrt pr) + sqrt pr
        tempL = 1 - (dkn * sqrt pr) + (0.5 * pr * (dkn**2))
        br = getBlockRatio (regenData i)
        pr = getPRN (gasData i)
        gam = getGAM (gasData i)
        (dkn, dtn, dpn) = (getNTPD i, getNTD i, getNPD i)

--- Result function
wn x l i = (tempE x l * (tempF x l - 1)) - tempG x l          -- Normalized input work
        where
        tempE x l = (dkn * l * (dpn**2) * (gam - 1) * br * cos2 x) / (4 * gam)
        tempF x l = (dtn * tan x) / (br * l * (gam - 1) * (1 + sqrt pr) * tempL)
        tempG x l = (dkn * l * (dpn**2) * sqrt pr * sin2 x) / (4 * gam * br * tempL)
        tempL = 1 - (dkn * sqrt pr) + (0.5 * pr * (dkn**2))
        br = getBlockRatio (regenData i)
        pr = getPRN (gasData i)
        gam = getGAM (gasData i)
        (dkn, dtn, dpn) = (getNTPD i, getNTD i, getNPD i)

cop x l = qcn x l / wn x l                                  -- Coefficient of performance

copMax i = (1 / (getNTD i)) - 1

fCOP x i
    | isNaN a       = 0
    | a < 0         = 0
    | a >= copMax i = 0
    | otherwise     = a
    where
    a = cop x ((4*pi*lrest/wl) - (2 * x))
    wl = getWavelength i
    lrest = getLengthRest i

optX x = (cm - fCOP x i) / cm where cm = copMax i
dist = 0.1
xn = bestRoot optX (dist, 1 - dist) acc                     -- Best root x-value
copAct = fCOP xn                                            -- Actual COP

---

tempH lb = cot (k*(lt - (lsph + (2*lc) + lb)))              -- Impedance of one side
tempI lb = (dr**2) * tan (k*(lb + lc))                      -- Impedance of the other side
optHI lb = abs (tempH lb - tempI lb)                        -- Impedance-matching fitness function
lmax = lt - ((2*lc) + lsph)                                 -- Maximum possible length for lb
bestl = bestRoot optHI (lmax/2, lmax) acc                   -- Best root x-value
lb = lmax - bestl                                           -- Because this is the bigger root
