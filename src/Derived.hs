-- Derived values, calculated based on the input constants
module Derived where

import Input
import Utility (sin2, cos2, cot, log10, bestRoot, acc)

-- Derived dimensions
st = (1  - br) * (hr/br)                -- mm               -- Stack "thickness"


lr = (2*lrest) - ((wl * xn) / pi)       -- mm               -- Regenerator length
lta = lrest - lr                        -- mm               -- Length of the initial tube

cAngRad = cang * (pi/180.0)             -- rad              -- Cone half-angle
lc = (d1 - d2) / (2.0 * tan cAngRad)    -- mm               -- Cone length

dr = d1/d2                              -- DL               -- Diameter ratio

r0 = d0/2                               -- mm               -- Radius of speaker tube
r1 = d1/2                               -- mm               -- Radius of initial tube
r2 = d2/2                               -- mm               -- Radius of smaller tube

lsph = (d1/2)                           -- mm               -- Semispherical endcap length
u = (10^8)*dp/(rho*sos)                 -- mm/s             -- Temporary variable
lhex = (u/omg) * sin(k*lrest)           -- mm               -- Heat exchanger length
lhc = lhex                              -- mm               -- Cold heat exchanger length
lhh = lhex*2                            -- mm               -- Hot heat exchanger length

wl = 4.0 * lt                           -- mm               -- Wavelength

xa0 = circleArea r0                     -- mm^2             -- Speaker cabinet cross-sectional area
xa1 = circleArea r1                     -- mm^2             -- Cross-sectional area 1
xa2 = circleArea r2                     -- mm^2             -- Cross-sectional area 2
xa3 = circleArea' spdsmall              -- mm^2             -- Cross-sectional area of speaker

vc = pi*lc*((r1**2)+(r2**2)+(r1*r2))/3  -- mm^3             -- Cone volume
vb = xa2*lb                             -- mm^3             -- Thin tube volume
vsph = (4/6)*pi*(lsph**3)               -- mm^3             -- Endcap volume
vhex = xa1*lhex                         -- mm^3             -- Heat exchanger volume
vta = xa1*lta                           -- mm^3             -- Volume of Tube A
vr = xa1*lr                             -- mm^3             -- Regenerator volume (total)
vra = xa1*lr*(1-br)                     -- mm^3             -- Regenerator volume (gas)

lrest = lt - ((2*lc)+lb+lsph)           -- mm               -- Length of the rest of the resonator

vrest = xa1*lrest                       -- mm^3             -- Volume thereof

dvmax = spsd*spxmax                     -- mm^3             -- Change in volume as the speaker cycles

vresti = vrest+dvmax                    -- mm^3             -- Volume at maximum negative speaker displacement (MNSD)
vrestf = vrest-dvmax                    -- mm^3             -- Volume at maximum positive speaker displacement (MPSD)
vtotal = vrest + (2*vc) + vsph + vb     -- mm^3             -- Total resonator volume
vtotali = vresti + (2*vc) + vsph + vb   -- mm^3             -- Total resonator volume at MSND
vtotalf = vrestf + (2*vc) + vsph + vb   -- mm^3             -- Total resonator volume at MSPD

ltotal = lbox + lt                      -- mm               -- Total length of device

dprms = dpn / (sqrt 2)                  -- DL               -- Dynamic RMS pressure
loud = 20 * ((log10 (dprms / 20)) + 11) -- dB SPL           -- Sound pressure in decibels relative to the interior sound threshold

-- Derived values

--- Thiele-Small stuff
vbox = (spvas / spalpha) + vspeaker     -- mm^3             -- Speaker cabinet volume
lbox = vbox/xa0                         -- mm               -- Speaker cabinet length

-- Constraints
--- Temporary variables to make the equation easier
tempL = 1 - (dkn * sqrt pr) + (0.5 * pr * (dkn**2))
tempA x = (dkn * (dpn**2) * sin x) / (8 * gam * (1 + pr) * tempL)
tempB x l = (dtn * tan x) / ((gam - 1) * br * l)
tempC = (1 + pr + sqrt pr) / (1 + sqrt pr)
tempD = 1 - (dkn * sqrt pr) + sqrt pr
--- Result function
qcn x l = tempA x * ((tempC * tempB x l) - tempD)           -- Normalized output heat

--- Temporary variables to make the equation easier
tempE x l = (dkn * l * (dpn**2) * (gam - 1) * br * cos2 x) / (4 * gam)
tempF x l = (dtn * tan x) / (br * l * (gam - 1) * (1 + sqrt pr) * tempL)
tempG x l = (dkn * l * (dpn**2) * sqrt pr * sin2 x) / (4 * gam * br * tempL)
--- Result function
wn x l = (tempE x l * (tempF x l - 1)) - tempG x l          -- Normalized input work

cop x l = qcn x l / wn x l                                  -- Coefficient of performance

copMax = (t - dt) / dt                                      -- Maximum COP based on Carnot cycle

fCOP x
    | isNaN a       = 0
    | a < 0         = 0
    | a >= copMax   = 0
    | otherwise     = a
    where
    a = cop x ((4*pi*lrest/wl) - (2 * x))

optX x = (copMax - fCOP x) / copMax
dist = 0.1
xn = bestRoot optX (dist, 1 - dist) acc                     -- Best root x-value
--xn = rootError optX (dist, 1 - dist) acc
copAct = fCOP xn                                            -- Actual COP

---

tempH lb = cot (k*(lt - (lsph + (2*lc) + lb)))              -- Impedance of one side
tempI lb = (dr**2) * tan (k*(lb + lc))                      -- Impedance of the other side
optHI lb = abs (tempH lb - tempI lb)                        -- Impedance-matching fitness function
lmax = lt - ((2*lc) + lsph)                                 -- Maximum possible length for lb
bestl = bestRoot optHI (lmax/2, lmax) acc                   -- Best root x-value
lb = lmax - bestl                                           -- Because this is the bigger root
