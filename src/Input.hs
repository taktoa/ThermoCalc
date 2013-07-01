-- Input constants; this is the only file you need to edit, generally
module Input where

import Utility (e)

-- Input variables
p = 5.0                                 -- bar              -- Pressure
t = 298.15                              -- K                -- Temperature
dt = 30.0                               -- K                -- Temperature differential

-- Input dimensions
lt = 650.0                              -- mm               -- Total resonator length
--- Values are for Schedule 40, NPS size 8, NPS size 3, and NPS size 1.5 pipe
d0 = 202.720                            -- mm               -- Speaker tube inside diameter
d1 = 77.920                             -- mm               -- Large tube inside diameter
d2 = 40.894                             -- mm               -- Small tube inside diameter
--- Set to 0 for idealized construction
fl = 0.0                                -- mm               -- Flange thickness
--- Values are for thin-wall 400 c/in^2 Celcor
hr = 0.2925                             -- mm               -- Regenerator hydraulic radius
br = 0.83                               -- DL               -- Blockage ratio
--- Shouldn't need to be changed, generally
ang = 9.0                               -- deg              -- Cone half-angle

-- Speaker Properties
-- These values are for the speaker at: http://goo.gl/EIr3c
-- Feel free to substitute your own Thiele-Small parameters
spPrms = 50.0                           -- W                -- RMS power handling
spPmax = 100.0                          -- W                -- Maximum power input
spfmin = 90.0                           -- Hz               -- Minimum frequency
spfmax = 3000.0                         -- Hz               -- Maximum frequency
spfres = 83.0                           -- Hz               -- Resonant frequency
sps = 89.0                              -- dB 1W / 1m       -- Speaker sensitivity
sprdc = 4.49                            -- ohm              -- DC coil resistance
spinduc = 0.00107                       -- H                -- Coil inductance
spqms = 3.24                            -- DL               -- Mechanical Q
spqes = 0.89                            -- DL               -- Electrical Q
spqts = 0.70                            -- DL               -- Total system Q
spvas = 7930000.0                       -- mm^3             -- Compliance volume
spxmax = 2.0                            -- mm               -- Speaker cone movement

-- Speaker Dimensions
splen = 70.0                            -- mm               -- Speaker length
spdsmall = 165.1                        -- mm               -- Diameter of active speaker area
spdscrew = 171.5                        -- mm               -- Diameter of speaker screws 
spdtotal = 187.5                        -- mm               -- Total speaker diameter

-- Constants
-- These are for nitrogen
cp = 1.040                              -- J/(g*K)          -- Constant-pressure specific heat
cv = 0.743                              -- J/(g*K)          -- Constant-volume specific heat
gam = cp/cv                             -- DL               -- Specific heat ratio

-- These are for helium
--cp = 5.193                              -- J/(g*K)          -- Constant-pressure specific heat
--cv = 3.116                              -- J/(g*K)          -- Constant-volume specific heat
--gam = cp/cv                             -- DL               -- Specific heat ratio

-- Regressions
-- These are for nitrogen
sos = 1000.0 * (186.77 + (0.55 * t))    -- mm/s             -- Speed of sound
kg = ((2.9929 `e` (-5)) * t) ** 0.775   -- W/(m*K)          -- Thermal conductivity
mu = ((1.8417 `e` (-5)) * t) ** 0.775   -- cP               -- Viscosity
rho = (1.242 * p) / ((3.61 * t) + 8.86) -- g/mL             -- Density

-- These are for helium
--sos = 1000.0 * (480.589 + (1.7875 * t)) -- mm/s             -- Speed of sound
--kg = ((2.38889 `e` (-4)) * t) ** 0.710  -- W/(m*K)          -- Thermal conductivity
--mu = ((7.96389 `e` (-6)) * t) ** 0.647  -- cP               -- Viscosity
--rho = (0.4791 * p) / t                  -- g/mL             -- Density
