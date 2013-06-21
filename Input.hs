-- Input constants; this is the only file you need to edit, generally
module Input where

import Utility (e)

-- Input variables
p = 10.0                                -- bar              -- Pressure
t = 298.15                              -- K                -- Temperature
dt = 10.0                               -- K                -- Temperature differential

-- Input dimensions
lt = 750.0                              -- mm               -- Total resonator length
d1 = 77.92                              -- mm               -- Large tube diameter
d2 = 40.894                             -- mm               -- Small tube diameter
fl = 10.0                               -- mm               -- Flange thickness
hr = 0.2925                             -- mm               -- Regenerator hydraulic radius
--hr = 0.015                              -- mm               -- Regenerator hydraulic radius
br = 0.83                               -- DL               -- Blockage ratio
--br = 0.75                               -- DL               -- Blockage ratio
ang = 9.0                               -- deg              -- Cone half-angle

-- Speaker Properties
-- These values are for the speaker at: http://goo.gl/DwMs7
-- Feel free to substitute your own Thiele-Small parameters
spPrms = 25.0                           -- W                -- RMS power handling
spPmax = 50.0                           -- W                -- Maximum power input
spfmin = 80.0                           -- Hz               -- Minimum frequency
spfmax = 6000.0                         -- Hz               -- Maximum frequency
spfres = 77.0                           -- Hz               -- Resonant frequency
sps = 86.0                              -- dB 1W / 1m       -- Speaker sensitivity
sprdc = 5.6                             -- ohm              -- DC coil resistance
spinduc = 0.60                          -- mH               -- Coil inductance
spqms = 2.43                            -- DL               -- Mechanical Q
spqes = 0.90                            -- DL               -- Electrical Q
spqts = 0.66                            -- DL               -- Total system Q
spvas = 5.1                             -- L                -- Compliance volume
spxmax = 1.5                            -- mm               -- Speaker cone movement

-- Speaker Dimensions
splen = 60.0                            -- mm               -- Speaker length
spdsmall = 115.0                        -- mm               -- Diameter of active speaker area
spdscrew = 127.0                        -- mm               -- Diameter of speaker screws 
spdtotal = 146.0                        -- mm               -- Total speaker diameter

-------------------------------------------------------------------------------------
-- If you are using helium as your working fluid, don't change anything below here --
-------------------------------------------------------------------------------------

-- Constants
-- These are for helium, and are temperature- and pressure-invariant
cp = 5.193                              -- J/(g*K)          -- Constant-pressure specific heat
cv = 3.116                              -- J/(g*K)          -- Constant-volume specific heat
gam = cp/cv                             -- DL               -- Specific heat ratio

-- Regressions
-- These are for helium
sos = 1000.0 * (480.589 + (1.7875 * t)) -- mm/s             -- Speed of sound
kg = ((2.38889 `e` (-4)) * t) ** 0.710  -- W/(m*K)          -- Thermal conductivity
mu = ((7.96389 `e` (-6)) * t) ** 0.647  -- cP               -- Viscosity
rho = ((47.91 * p) + 0.1472) / (1000*t) -- g/mL             -- Density
pr = (mu*cp)/kg                         -- DL               -- Prandtl number
