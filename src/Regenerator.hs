module Regenerator where

pr = (mu*cp)/kg                         -- DL               -- Prandtl number
mach = (100000000*dp)/(rho*(sos**2))    -- Mach             -- Mach number of resonator flow
f = sos / wl                            -- hertz            -- Frequency
omg = 2*pi*f                            -- rad/s            -- Rotational frequency
k = (2*pi) / wl                         -- mm^-1            -- Normalization factor
dp = dpn*p                              -- bar              -- Absolute pressure differential
dtn = dt / t                            -- DL               -- Relative temperature differential
dpn = (vtotali-vtotalf)/(2*vtotal)      -- DL               -- Relative pressure differential
dk = sqrt (kg / (rho*cp*pi*f))          -- mm               -- Thermal penetration depth
dkn = dk / hr                           -- DL               -- Normalized thermal penetration depth
dv = sqrt (mu / (rho*pi*f))             -- mm               -- Viscous penetration depth
dvn = dv / hr                           -- DL               -- Normalized viscous penetration depth
