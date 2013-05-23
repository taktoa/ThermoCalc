-- Input constants; this is the only file you need to edit, generally
module Input where

import Utility (e)

-- Input variables
p = 10.0                                -- bar              -- pressure
t = 298.15                              -- K                -- temperature
dt = 10.0                               -- K                -- temperature differential

-- Input dimensions
lt = 1000.0                             -- mm               -- total resonator length
d1 = 83.41                              -- mm               -- large tube diameter
d2 = 44.58                              -- mm               -- small tube diameter
hr = 0.2925                             -- mm               -- regenerator hydraulic radius
st = 0.06                               -- mm               -- proxy for blockage ratio

-- Speaker Properties
-- These values are for the speaker at:
-- http://www.parts-express.com/pe/showdetl.cfm?partnumber=299-973
-- Feel free to substitute your own Thiele-Small parameters
spPrms = 25                             -- W                -- RMS power handling
spPmax = 50                             -- W                -- Maximum power input
spfmin = 80                             -- Hz               -- Minimum frequency
spfmax = 6000                           -- Hz               -- Maximum frequency
spfres = 77                             -- Hz               -- Resonant frequency
sps = 86                                -- dB 1W / 1m       -- Speaker sensitivity
sprdc = 5.6                             -- ohm              -- DC coil resistance
spinduc = 0.60                          -- mH               -- Coil inductance
spqms = 2.43                            -- DL               -- Mechanical Q
spqes = 0.90                            -- DL               -- Electrical Q
spqts = 0.66                            -- DL               -- Total system Q
spvas = 5.1                             -- L                -- Compliance volume
spxmax = 1.5                            -- mm               -- Speaker cone movement

-- Speaker Dimensions
splen = 60                              -- mm               -- length of speaker
spdsmall = 115                          -- mm               -- diameter of active speaker area
spdscrew = 127                          -- mm               -- diameter of speaker screws 
spdtotal = 146                          -- mm               -- total speaker diameter

-------------------------------------------------------------------------------------
-- If you are using helium as your working fluid, don't change anything below here --
-------------------------------------------------------------------------------------

-- Constants
-- These are for helium, and are temperature- and pressure-invariant
cp = 5.193                              -- J/(g*K)          -- constant-pressure specific heat
cv = 3.116                              -- J/(g*K)          -- constant-volume specific heat
gam = cp/cv                             -- DL               -- specific heat ratio

-- Regressions
-- These are for helium
sos = 1000.0 * (480.589 + (1.7875 * t)) -- mm/s             -- speed of sound
kg = ((2.38889 `e` (-4)) * t) ** 0.710  -- W/(m*K)          -- thermal conductivity
mu = ((7.96389 `e` (-6)) * t) ** 0.647  -- cP               -- viscosity
rho = ((47.90903 * p) + 0.1472) / t     -- g/mL             -- density
pr = (mu*cp)/kg                         -- DL               -- Prandtl number
