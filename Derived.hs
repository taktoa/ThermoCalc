-- Derived values, calculated based on the input constants
module Derived (br, dr, r1, r2, lsph, lhex, lc, lb, wl, xa1,
                xa2, mach, f, k, dt, dtn, dp, dpn, dk, dkn,
                dv, dvn, qcn, wn, cop, copMax, acc) where

import Input
import Utility (sin2, cos2, cot)
import Data.Function (on)
import Data.List (minimumBy)

-- Derived dimensions
br = hr / (hr + st)                     -- DL       -- Blockage ratio
dr = d1/d2                              -- DL       -- Diameter ratio
r1 = d1/2                               -- mm       -- Radius 1
r2 = d2/2                               -- mm       -- Radius 2
lsph = d1/2                             -- mm       -- Semispherical endcap length
u = 100000000*dp/(rho*sos)              -- mm/s     -- Temporary variable
lhex = (u/omg) * sin(k*lrest)           -- mm       -- Heat exchanger length
theta = 9.0                             -- deg      -- Cone half-angle
thetaRad = theta * (pi/180)             -- rad      -- Cone half-angle
lc = (d1 - d2) / (2.0 * tan thetaRad)   -- mm       -- Cone length
wl = 4.0 * lt                           -- mm       -- Wavelength
xa1 = pi * (r1 ** 2.0)                  -- mm^2     -- Cross-sectional area 1
xa2 = pi * (r2 ** 2.0)                  -- mm^2     -- Cross-sectional area 2
vc = pi*lb*((r1**2)+(r2**2)+(r1*r2))/3  -- mm^3     -- Cone volume
vb = xa2*lb                             -- mm^3     -- Thin tube volume
vsph = (4/3)*pi*(lsph**3)               -- mm^3     -- Endcap volume
lrest = lt - ((2*lc)+lb+lsph)           -- mm       -- Length of the rest of the resonator
vrest = xa1*lrest                       -- mm^3     -- Volume thereof
vresti = xa1*(lrest+spxmax)             -- mm^3     -- Volume at maximum negative speaker displacement (MNSD)
vrestf = xa1*(lrest-spxmax)             -- mm^3     -- Volume at maximum positive speaker displacement (MPSD)
vtotal = vrest + (2*vc) + vsph + vb     -- mm^3     -- Total resonator volume
vtotali = vresti + (2*vc) + vsph + vb   -- mm^3     -- Total resonator volume at MSND
vtotalf = vrestf + (2*vc) + vsph + vb   -- mm^3     -- Total resonator volume at MSPD

-- Derived values
mach = (100000000*dp)/(rho*(sos**2))    -- Mach     -- Mach number of resonator flow
f = sos / wl                            -- hertz    -- Frequency
omg = 2*pi*f                            -- rad/s    -- Rotational frequency
k = (2*pi) / wl                         -- mm^-1    -- Normalization factor
dp = dpn*p                              -- bar      -- Absolute pressure differential
dtn = dt / t                            -- DL       -- Relative temperature differential
dpn = (vtotali-vtotalf)/vtotal          -- DL       -- Relative pressure differential
dk = sqrt (kg / (rho*cp*pi*f))          -- mm       -- Thermal penetration depth
dkn = dk / hr                           -- DL       -- Normalized thermal penetration depth
dv = sqrt (mu / (rho*pi*f))             -- mm       -- Viscous penetration depth
dvn = dv / hr                           -- DL       -- Normalized viscous penetration depth

-- Constraints
--- Temporary variables to make the equation easier
tempL = 1 - (dkn * sqrt pr) + (0.5 * pr * (dkn**2))
tempA x = (dkn * (dpn**2) * sin x) / (8 * gam * (1 + pr) * tempL)
tempB x l = (dtn * tan x) / ((gam - 1) * br * l)
tempC = (1 + pr + sqrt pr) / (1 + sqrt pr)
tempD = 1 - (dkn * sqrt pr) + sqrt pr
--- Result function
qcn x l = tempA x * ((tempC * tempB x l) - tempD)   -- Normalized output heat

--- Temporary variables to make the equation easier
tempE x l = (dkn * l * (dpn**2) * (gam - 1) * br * cos2 x) / (4 * gam)
tempF x l = (dtn * tan x) / (br * l * (gam - 1) * (1 + sqrt pr) * tempL)
tempG x l = (dkn * l * (dpn**2) * sqrt pr * sin2 x) / (4 * gam * br * tempL)
--- Result function
wn x l = (tempE x l * (tempF x l - 1)) - tempG x l  -- Normalized input work

cop x l = qcn x l / wn x l                          -- Coefficient of performance

---

tempH lb = cot (k*(lt - (lsph + (2*lc) + lb)))      -- Impedance of one side
tempI lb = (dr**2) * tan (k*(lb + lc))              -- Impedance of the other side
optHI lb = abs (tempH lb - tempI lb)                -- Impedance-matching fitness function
acc = 0.01                                          -- Accuracy of the root-finder
lmax = lt - ((2*lc) + lsph)                         -- Maximum possible length for lb
brckt = (lmax/2, lmax)                              -- Get the bigger root
findR f (m,n) d = [x | x <- [m,(m+d)..n], f x < d]  -- Naive root finder function
rootXs = findR optHI brckt acc                      -- Find the root x-values
rootYs = map optHI rootXs                           -- Find the fitness of each root
zrootYs = zip [0 .. length rootYs] rootYs           -- Add indices to each root
index = fst (minimumBy (compare `on` snd) zrootYs)  -- Find the index of the best root
root = rootXs !! index                              -- Best root x-value
lb = lmax - root                                    -- Because this is the bigger root

---

copMax = (t - dt) / dt                              -- Maximum COP based on Carnot cycle
