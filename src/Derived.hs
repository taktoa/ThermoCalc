-- Derived values, calculated based on the input constants
module Derived where

import Input
import Utility (sin2, cos2, cot, bestRoot, acc)

-- Derived dimensions
lr = (2*lrest) - ((wl * xn) / pi)
lta = lrest - lr
st = (1  - br) * (hr/br)                -- mm               -- Stack "thickness"
dr = d1/d2                              -- DL               -- Diameter ratio
r0 = d0/2                               -- mm               -- Radius of speaker tube
r1 = d1/2                               -- mm               -- Radius of initial tube
r2 = d2/2                               -- mm               -- Radius of smaller tube
lsph = (d1/2) + fl                      -- mm               -- Semispherical endcap length
u = 100000000.0*dp/(rho*sos)            -- mm/s             -- Temporary variable
lhex = (u/omg) * sin(k*lrest)           -- mm               -- Heat exchanger length
lhc = lhex                              -- mm               -- Cold heat exchanger length
lhh = lhex*2                            -- mm               -- Hot heat exchanger length
angRad = ang * (pi/180.0)               -- rad              -- Cone half-angle
uc = (d1 - d2) / (2.0 * tan angRad)     -- mm               -- Unadjusted cone length
lc = uc + (2*fl)                        -- mm               -- Cone length, adjusting for flanges
wl = 4.0 * lt                           -- mm               -- Wavelength
xa0 = pi * (r0 ** 2.0)                  -- mm^2             -- Speaker cabinet cross-sectional area
xa1 = pi * (r1 ** 2.0)                  -- mm^2             -- Cross-sectional area 1
xa2 = pi * (r2 ** 2.0)                  -- mm^2             -- Cross-sectional area 2
xa3 = pi * ((spdsmall / 2.0) ** 2.0)    -- mm^2             -- Cross-sectional area of speaker
vc = pi*lc*((r1**2)+(r2**2)+(r1*r2))/3  -- mm^3             -- Cone volume
vb = xa2*lb                             -- mm^3             -- Thin tube volume
vsph = (4/6)*pi*(lsph**3)               -- mm^3             -- Endcap volume
vhex = xa1*lhex                         -- mm^3             -- Heat exchanger volume
vta = xa1*lta                           -- mm^3             -- Volume of Tube A
vr = xa1*lr                             -- mm^3             -- Regenerator volume (total)
vra = xa1*lr*(1-br)                     -- mm^3             -- Regenerator volume (gas)
lrest = lt - ((2*lc)+lb+lsph)           -- mm               -- Length of the rest of the resonator
vrest = xa1*lrest                       -- mm^3             -- Volume thereof
dvrest = spsd*spxmax                    -- mm^3             -- Change in volume as the speaker cycles
vresti = vrest+dvrest                   -- mm^3             -- Volume at maximum negative speaker displacement (MNSD)
vrestf = vrest-dvrest                   -- mm^3             -- Volume at maximum positive speaker displacement (MPSD)
vtotal = vrest + (2*vc) + vsph + vb     -- mm^3             -- Total resonator volume
vtotali = vresti + (2*vc) + vsph + vb   -- mm^3             -- Total resonator volume at MSND
vtotalf = vrestf + (2*vc) + vsph + vb   -- mm^3             -- Total resonator volume at MSPD

-- Derived values
pr = (mu*cp)/kg                         -- DL               -- Prandtl number
mach = (100000000*dp)/(rho*(sos**2))    -- Mach             -- Mach number of resonator flow
f = sos / wl                            -- hertz            -- Frequency
omg = 2*pi*f                            -- rad/s            -- Rotational frequency
k = (2*pi) / wl                         -- mm^-1            -- Normalization factor
dp = dpn*p                              -- bar              -- Absolute pressure differential
dtn = dt / t                            -- DL               -- Relative temperature differential
dpn = (vtotali-vtotalf)/vtotal          -- DL               -- Relative pressure differential
dk = sqrt (kg / (rho*cp*pi*f))          -- mm               -- Thermal penetration depth
dkn = dk / hr                           -- DL               -- Normalized thermal penetration depth
dv = sqrt (mu / (rho*pi*f))             -- mm               -- Viscous penetration depth
dvn = dv / hr                           -- DL               -- Normalized viscous penetration depth

--- Thiele-Small stuff
sprsmall = spdsmall/2                   -- mm               -- Radius of active speaker area
sprscrew = spdscrew/2                   -- mm               -- Radius of speaker screws
sprtotal = spdtotal/2                   -- mm               -- Total speaker radius
spomgres = 2 * pi * spfres              -- rad/s            -- Rotational resonant frequency of speaker
spsd = pi * sprsmall * l                -- mm^2             -- Speaker cone projected area
    where l = sqrt (((splen/2)**2) + (sprsmall**2))
spcms = (10**6) * spvas / a             -- m/N              -- Speaker compliance
    where a = rho * (sos**2) * (spsd**2)
spbl = sqrt (sprdc / a)                 -- T * m            -- Speaker force factor
    where a = spomgres * spqes * spcms
spmms = (spbl**2) * a                   -- kg               -- Speaker moving mass, including air
    where a = spqes / (spomgres * sprdc)
sprms = spomgres * spmms / spqms        -- N*s/m            -- Speaker mechanical resistance
spkc = 1 / spcms                        -- N/m              -- Speaker spring constant
spalpha = ((f/spfres)**2) - 1           -- DL               -- Speaker cabinet alpha
vmagnet = pi*splen*(spdtotal**2.0)/32   -- mm^3             -- Rough approximation of speaker magnet volume
vscone = 7*pi*splen*(spdtotal**2.0)/96  -- mm^3             -- Rough approximation of speaker cone volume
vspeaker = vmagnet + vscone             -- mm^3             -- Rough approximation of speaker volume
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
copAct = fCOP xn                                            -- Actual COP

---

tempH lb = cot (k*(lt - (lsph + (2*lc) + lb)))              -- Impedance of one side
tempI lb = (dr**2) * tan (k*(lb + lc))                      -- Impedance of the other side
optHI lb = abs (tempH lb - tempI lb)                        -- Impedance-matching fitness function
lmax = lt - ((2*lc) + lsph)                                 -- Maximum possible length for lb
bestl = bestRoot optHI (lmax/2, lmax) acc                   -- Best root x-value
lb = lmax - bestl                                           -- Because this is the bigger root
