module Speaker where

import Utility
import Input
import qualified Prelude
import Prelude (Double)
import Numeric.Units.Dimensional.Prelude
import WorkingFluid

data SpeakerData = SpeakerData {
                    measureData     :: GasData,
                    rawResData      :: SpeakerResData,
                    rawElecData     :: SpeakerElecData,
                    rawDimData      :: SpeakerDimData
                    }

data SpeakerResData = SpeakerResData {
                    resFreq         :: Frequency Double,
                    minFreq         :: Frequency Double,
                    maxFreq         :: Frequency Double,
                    complianceVol   :: Volume Double,
                    diaphragmMass   :: Mass Double,
                    qMech           :: Dimensionless Double,
                    qElec           :: Dimensionless Double
                    }

data SpeakerElecData = SpeakerElecData {
                    resistance      :: Resistance Double,
                    inductance      :: Inductance Double,
                    maxPower        :: Power Double,
                    rmsPower        :: Power Double,
                    impedance       :: Resistance Double
                    }

data SpeakerDimData = SpeakerDimData {
                    innerDiam       :: Length Double,
                    screwDiam       :: Length Double,
                    totalDiam       :: Length Double,
                    thickness       :: Length Double,
                    maxDisp         :: Length Double
                    }

adjustedResData :: SpeakerData -> InputData -> SpeakerResData
adjustedResData s i = SpeakerResData rf minf maxf comvol mmd qm qe
        where
        oldGas = measureData s
        newGas = gasData i
        sosRatio = (getSV newGas) / (getSV oldGas)
        rhoRatio = (getRHO newGas) / (getRHO oldGas)
        SpeakerResData orf ominf omaxf ocomvol mmd qm qe = rawResData s
        [rf, minf, maxf] = map (\x -> sosRatio * x) [orf, ominf, omaxf]
        comvol = ocomvol * rhoRatio

adjustedElecData :: SpeakerData -> InputData -> SpeakerElecData
adjustedElecData s i = rawElecData s                                    -- No changes to data

adjustedDimData :: SpeakerData -> InputData -> SpeakerDimData
adjustedDimData s i = rawDimData s                                      -- No changes to data

adjDimGet f s input = f (adjustedDimData s input)
adjResGet f s input = f (adjustedResData s input)
adjElcGet f s input = f (adjustedElecData s input)

getResData :: SpeakerData -> InputData -> SpeakerResData
getResData = adjustedResData

getElecData :: SpeakerData -> InputData -> SpeakerElecData
getElecData = adjustedElecDataData

getDimData :: SpeakerData -> InputData -> SpeakerDimData
getDimData = adjustedDimData

-- Dimension accessor functions
getInnerD = adjDimGet innerDiam
getScrewD = adjDimGet screwDiam
getTotalD = adjDimGet totalDiam
getThickness = adjDimGet thickness
getMaxDisp = adjDimGet maxDisp

-- Electrical accessor functions
getElecResist = adjElcGet resistance
getElecInduct = adjElcGet inductance
getElecImped = adjElcGet impedance
getMaxPower = adjElcGet maxPower
getRMSPower = adjElcGet rmsPower

-- Resonance accessor functions
getResFreq = adjResGet resFreq
getMinFreq = adjResGet minFreq
getMaxFreq = adjResGet maxFreq
getCompVol = adjResGet complianceVol
getDiaMass = adjResGet diaphragmMass
getQElec = adjResGet qElec
getQMech = adjResGet qMech

getQTotal :: SpeakerData -> InputData -> Dimensionless Double
getQTotal s i = (qe * qm) / (qe + qm)                                   -- Total Q
        where
        qe = getQElec s i
        qm = getQMech s i
getConeSurf :: SpeakerData -> InputData -> Area Double
getConeSurf s i = (pi/_8) * (di * sqrt ((squ len) + (squ di)))          -- Approximation for surface area of cone
        where
        di = getInnerDiam s i
        len = getThickness s i
getCompliance :: SpeakerData -> InputData -> Compliance Double
getCompliance s i = cv / (rho * (squ sos) * (squ sd))                   -- Speaker suspension compliance
        where
        cv = getCompVol s i
        rho = getRHO (gasData i)
        sos = getSV (gasData i)
        sd = getConeSurf s i
getBLValue :: SpeakerData -> InputData -> BLValue Double
getBLValue s i = sqrt (rdc / (_2 * pi * fres * cms * qes))              -- B*l Thiele-Small value
        where
        rdc = getElecResist s i
        fres = getResFreq s i
        qes = getQElec s i
        cms = getCompliance s i
getMovMass :: SpeakerData -> InputData -> Mass Double
getMovMass s i = (squ bl) * (qes / (_2 * pi * fres * rdc))              -- Moving mass of system, including air
        where
        bl = getBLValue s i
        qes = getQElec s i
        fres = getResFreq s i
        rdc = getElecResist s i
getMechResist s i = _2 * pi * fres * mms / qms                          -- Mechanical resistance of speaker
        where
        fres = getResFreq s i
        mms = getMovMassAir s i
        qms = getQMech s i
getSpringConstant :: SpeakerData -> InputData -> SpringConstant Double
getSpringConstant s i = _1 / (getCompliance s i)                        -- Spring constant of the speaker
getSpeakerVolume :: SpeakerData -> InputData -> Volume Double
getSpeakerVolume s i = n * pi * len * (squ dt)                          -- Rough approximation of speaker volume
        where
        n = ((5 !/ 48) *~ one)
        dt = getTotalDiam s i
        len = getThickness s i
getAlphaValue :: SpeakerData -> InputData -> Dimensionless Double
getAlphaValue s i = (squ (f/fres)) - _1
        where
        fres = getResFreq s i
        f = getFrequency i
getBoxVolume :: SpeakerData -> InputData -> Volume Double
getBoxVolume s i = (vas / alpha) + vs
        where
        vas = getCompVol s i
        alpha = getAlphaValue s i
        vs = getSpeakerVolume s i
getBoxLength :: SpeakerData -> InputData -> Length Double
getBoxLength s i = vb / xa
        where
        vb = getBoxVolume s i
        xa = (pi/_4) * (squ (speakBoxDiam (dimData i)))
