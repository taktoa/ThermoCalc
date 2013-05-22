module Derived (br, dr, r1, r2, lsph, lhex, lc, lb, wl, xa1,
                xa2, mach, f, k, dt, dtn, dp, dpn, dk, dkn,
                dv, dvn, qcn, wn, cop, copMax, acc) where

import Input
import Data.Function (on)
import Data.List (minimumBy)

-- Useful functions
sin2 x = sin x ** 2.0
cos2 x = cos x ** 2.0
cot x = 1.0 / tan x

-- Derived dimensions
br = hr / (hr + st)                     -- dimensionless
dr = d1/d2                              -- dimensionless
r1 = d1/2                               -- millimeter
r2 = d2/2                               -- millimeter
lsph = d1/2                             -- millimeter
u = 100000000*dp/(rho*sos)              -- mm/s
lhex = (u/omg) * sin(k*lrest)           -- millimeter
lc = (d1 - d2) / (2.0 * tan 0.15708)    -- millimeter
wl = 4.0 * lt                           -- millimeter
xa1 = pi * (r1 ** 2.0)                  -- square millimeter
xa2 = pi * (r2 ** 2.0)                  -- square millimeter

vc = pi*lb*((r1**2)+(r2**2)+(r1*r2))/3  -- mm^3
vb = xa2*lb                             -- mm^3
vsph = (4/3)*pi*(lsph**3)               -- mm^3
lrest = lt - ((2*lc)+lb+lsph)           -- millimeters
vrest = xa1*lrest                       -- mm^3
vresti = xa1*(lrest+spxmax)             -- mm^3
vrestf = xa1*(lrest-spxmax)             -- mm^3
vtotal = vrest + (2*vc) + vsph + vb     -- mm^3
vtotali = vresti + (2*vc) + vsph + vb   -- mm^3
vtotalf = vrestf + (2*vc) + vsph + vb   -- mm^3

-- Derived values
mach = (100000000*dp)/(rho*(sos**2))    -- Mach
f = sos / wl                            -- hertz
omg = 2*pi*f                            -- rps
k = (2*pi) / wl                         -- mm^-1
dp = dpn*p                              -- bar
dtn = dt / t                            -- dimensionless
dpn = (vtotali-vtotalf)/vtotal          -- dimensionless
dk = sqrt (kg / (rho*cp*pi*f))          -- millimeters
dkn = dk / hr                           -- dimensionless
dv = sqrt (mu / (rho*pi*f))             -- millimeters
dvn = dv / hr                           -- dimensionless

-- Constraints
tempL = 1 - (dkn * sqrt pr) + (0.5 * pr * (dkn**2))
tempA x = (dkn * (dpn**2) * sin x) / (8 * gam * (1 + pr) * tempL)
tempB x l = (dtn * tan x) / ((gam - 1) * br * l)
tempC = (1 + pr + sqrt pr) / (1 + sqrt pr)
tempD = 1 - (dkn * sqrt pr) + sqrt pr
qcn x l = tempA x * ((tempC * tempB x l) - tempD)

tempE x l = (dkn * l * (dpn**2) * (gam - 1) * br * cos2 x) / (4 * gam)
tempF x l = (dtn * tan x) / (br * l * (gam - 1) * (1 + sqrt pr) * tempL)
tempG x l = (dkn * l * (dpn**2) * sqrt pr * sin2 x) / (4 * gam * br * tempL)
wn x l = (tempE x l * (tempF x l - 1)) - tempG x l

cop x l = qcn x l / wn x l

---

tempH lb = cot (k*(lt - (lsph + (2*lc) + lb)))
tempI lb = (dr**2) * tan (k*(lb + lc))
optHI lb = abs (tempH lb - tempI lb)
acc = 0.01
lmax = lt - ((2*lc) + lsph)
brckt = (lmax/2, lmax)
findRoots f start stop step = [x | x <- [start, start+step .. stop], abs (f x) < step]
rootXs = uncurry (findRoots optHI) brckt acc
rootYs = map optHI rootXs
zippedrootYs = zip [0 .. length rootYs] rootYs
index = fst (minimumBy (compare `on` snd) zippedrootYs)
lb = lmax - (rootXs !! index)

---

copMax = (t - dt) / dt
