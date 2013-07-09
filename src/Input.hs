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
getWavelength i = _4 * totalLength (dimData i)

getFrequency :: InputData -> Frequency Double
getFrequency i = sos / getWavelength i
        where
        sos = getSV (gasData i)

getWavenumber :: InputData -> WaveNumber Double
getWavenumber i = _2 * pi / getWavelength i

getRotFrequency :: InputData -> Frequency Double
getRotFrequency i = _2 * pi * getFrequency i

getTPD :: InputData -> Length Double
getTPD i = sqrt (kg / (rho * cp * pi * f))
        where
        gd = gasData i
        (kg, rho, cp) = (getTC gd, getRHO gd, getCP gd)
        f = getFrequency i

getNTPD :: InputData -> DimlessDouble
getNTPD i = getTPD i / getHydRadius (regenData i)

getVPD :: InputData -> Length Double
getVPD i = sqrt (mu / (pi * rho * f))
        where
        gd = gasData i
        (mu, rho) = (getDV gd, getRHO gd)
        f = getFrequency i

getNVPD :: InputData -> DimlessDouble
getNVPD i = getVPD i / getHydRadius (regenData i)

getTD :: InputData -> Temperature Double
getTD i = getTempDiff (regenData i)

getNTD :: InputData -> DimlessDouble
getNTD i = getTD i / getTemp (gasData i)
