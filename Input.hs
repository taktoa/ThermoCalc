module Input where

-- Useful functions
e a b = a * (10.0 ** b)

-- Input variables
p = 10.0                                -- bar
t = 298.15                              -- kelvin
dt = 10.0                               -- kelvin

-- Input dimensions
lt = 1000.0                             -- millimeter
d1 = 83.41                              -- millimeter
d2 = 44.58                              -- millimeter
y0 = 0.2925                             -- millimeter
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
