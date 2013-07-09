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
                    
getGasData :: InputData -> GasData
getGasData = gasData

getDimData :: InputData -> DimData
getDimData = dimData

getRegenData :: InputData -> RegenData
getRegenData = regenData

getWavelength :: InputData -> Length Double
getWavelength i = _4 * (totalLength (dimData i))

getFrequency :: InputData -> Frequency Double
getFrequency i = sos / (getWavelength i)
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

getNTPD :: InputData -> DimlessDouble
getNTPD i = (getTPD i) / (getHydRadius (regenData i))

getVPD :: InputData -> Length Double
getVPD i = sqrt (mu / (pi * rho * f))
        where
        gd = gasData i
        (mu, rho) = (getDV gd, getRHO gd)
        f = getFrequency i

getNVPD :: InputData -> DimlessDouble
getNVPD i = (getVPD i) / (getHydRadius (regenData i))

getTD :: InputData -> Temperature Double
getTD i = getTempDiff (regenData i)

getNTD :: InputData -> DimlessDouble
getNTD i = (getTD i) / (getTemp (gasData i))

        
---- Input dimensions
--lt = 750.0                              -- mm               -- Total resonator length

----- Values are for Schedule 40, NPS size 8, NPS size 3, and NPS size 1.5 pipe
--d0 = 202.720                            -- mm               -- Speaker tube inside diameter
--d1 = 77.920                             -- mm               -- Large tube inside diameter
--d2 = 40.894                             -- mm               -- Small tube inside diameter

----- Values are for thin-wall 400 c/in^2 Celcor
--hr = 0.2925                             -- mm               -- Regenerator hydraulic radius
--br = 0.83                               -- DL               -- Blockage ratio
