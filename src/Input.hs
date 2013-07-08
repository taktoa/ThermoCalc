-- Input constants; this is the only file you need to edit, generally
module Input where

import Utility 
import WorkingFluid
import Regenerator
import Numeric.Units.Dimensional.Prelude
import qualified Prelude

data InputData = InputData {
                    gasData         :: GasData,
                    dimData         :: DimData,
                    regenData       :: RegenData
                    }

data DimData = DimData {
                    totalLength     :: Length Double,
                    speakBoxDiam    :: Length Double,
                    bigtubeDiam     :: Length Double,
                    smalltubeDiam   :: Length Double
                    }

getWavelength :: InputData -> Length Double
getWavelength i = _4 * (totalLength (dimData i))

getFrequency :: InputData -> Frequency Double
getFrequency i = sos / (_4 * (getWavelength i))
        where
        sos = getSV (gasData i)

getWavenumber :: InputData -> WaveNumber Double
getWavenumber i = (_2 * pi) / (getWavelength i)

getRotFrequency :: InputData -> Frequency Double
getRotFrequency i = (_2 * pi) * (getFrequency i)

getTPD :: InputData -> Length Double
getTPD i = sqrt (kg / (rho * cp * pi * f))
        where
        gd = gasData i
        (kg, rho, cp) = (getTC gd, getRHO gd, getCP gd)
        f = getFrequency i

getNTPD :: InputData -> Dimensionless Double
getNTPD i = (getTPD i) / (getHydRadius (regenData i))

getVPD :: InputData -> Length Double
getVPD i = sqrt (mu / (pi * rho * f))
        where
        gd = gasData i
        (mu, rho) = (getDV gd, getRHO gd)
        f = getFrequency i

getNVPD :: InputData -> Dimensionless Double
getNVPD i = (getVPD i) / (getHydRadius (regenData i))

--mach = (100000000*dp)/(rho*(sos**2))    -- Mach             -- Mach number of resonator flow
--dp = dpn*p                              -- bar              -- Absolute pressure differential
--dtn = dt / t                            -- DL               -- Relative temperature differential
--dpn = (vtotali-vtotalf)/(2*vtotal)      -- DL               -- Relative pressure differential

---- Input dimensions
--lt = 750.0                              -- mm               -- Total resonator length

----- Values are for Schedule 40, NPS size 8, NPS size 3, and NPS size 1.5 pipe
--d0 = 202.720                            -- mm               -- Speaker tube inside diameter
--d1 = 77.920                             -- mm               -- Large tube inside diameter
--d2 = 40.894                             -- mm               -- Small tube inside diameter

----- Values are for thin-wall 400 c/in^2 Celcor
--hr = 0.2925                             -- mm               -- Regenerator hydraulic radius
--br = 0.83                               -- DL               -- Blockage ratio

----- Shouldn't need to be changed, generally
--cang = 9.0                              -- deg              -- Cone half-angle
