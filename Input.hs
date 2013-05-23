module Input where

import Utility (e)

-- Input variables
p = 10.0                                -- bar              -- pressure
t = 298.15                              -- kelvin           -- temperature
dt = 10.0                               -- kelvin           -- temperature differential

-- Input dimensions
lt = 1000.0                             -- millimeter       -- total resonator length
d1 = 83.41                              -- millimeter       -- large tube diameter
d2 = 44.58                              -- millimeter       -- small tube diameter
hr = 0.2925                             -- millimeter       -- regenerator hydraulic radius
st = 0.06                               -- millimeter       -- proxy for blockage ratio

-- Speaker Properties
-- These values are for the speaker at:
-- http://www.parts-express.com/pe/showdetl.cfm?partnumber=299-973
-- Feel free to substitute your own Thiele-Small parameters
spPrms = 25                             -- watt             -- RMS power handling
spPmax = 50                             -- watt             -- maximum power input
spfmin = 80                             -- hertz            -- minimum frequency
spfmax = 6000                           -- hertz            -- maximum frequency
spfres = 77                             -- hertz            -- resonant frequency
sps = 86                                -- dB 1W / 1m       -- speaker sensitivity
sprdc = 5.6                             -- ohm              -- DC coil resistance
spinduc = 0.60                          -- millihenry       -- coil inductance
spqms = 2.43                            -- dimensionless    -- mechanical Q
spqes = 0.90                            -- dimensionless    -- electrical Q
spqts = 0.66                            -- dimensionless    -- total system Q
spvas = 5.1                             -- liter            -- compliance volume
spxmax = 1.5                            -- millimeter       -- speaker cone movement

-- Speaker Dimensions
splen = 60                              -- millimeter       -- length of speaker
spdsmall = 115                          -- millimeter       -- diameter of active speaker area
spdscrew = 127                          -- millimeter       -- diameter of speaker screws 
spdtotal = 146                          -- millimeter       -- total speaker diameter

-------------------------------------------------------------------------------------
-- If you are using helium as your working fluid, don't change anything below here --
-------------------------------------------------------------------------------------

-- Constants
-- These are for helium, and are temperature- and pressure-invariant
cp = 5.193                              -- J / (g*K)        -- constant-pressure specific heat
cv = 3.116                              -- J / (g*K)        -- constant-volume specific heat
gam = cp/cv                             -- dimensionless    -- specific heat ratio

-- Regressions
-- These are for helium
sos = 1000.0 * (480.589 + (1.7875 * t)) -- mm/s             -- speed of sound
kg = ((2.38889 `e` (-4)) * t) ** 0.710  -- W / (m*K)        -- thermal conductivity
mu = ((7.96389 `e` (-6)) * t) ** 0.647  -- centipoise       -- viscosity
rho = ((47.90903 * p) + 0.1472) / t     -- g/mL             -- density
pr = (mu*cp)/kg                         -- dimensionless    -- Prandtl number
