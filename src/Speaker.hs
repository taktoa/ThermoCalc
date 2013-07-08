module Speaker where

import Numeric.Units.Dimensional
import WorkingFluid

type Resistance = ElectricResistance

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
                    qMech           :: Double,
                    qElec           :: Double
                    }

data SpeakerElecData = SpeakerElecData {
                    resistance      :: Resistance Double,
                    inductance      :: Inductance Double,
                    capacitance     :: Capacitance Double,
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

adjustedResData :: SpeakerData -> GasData -> SpeakerResData
adjustedResData input newGas = SpeakerResData rf minf maxf comvol qm qe
        where
        oldGas = measureData input
        sosRatio = (getSV newGas) / (getSV oldGas)
        rhoRatio = (getRHO newGas) / (getRHO oldGas)
        SpeakerResData orf ominf omaxf ocomvol qm qe = rawResData input
        [rf, minf, maxf] = map (\x -> sosRatio * x) [orf, ominf, omaxf]
        comvol = ocomvol * rhoRatio

adjustedElecData :: SpeakerData -> GasData -> SpeakerElecData
adjustedElecData = rawElecData                                          -- No changes to data

adjustedDimData :: SpeakerData -> GasData -> SpeakerDimData
adjustedDimData = rawDimData                                            -- No changes to data

adjDimGet f dat gas = f (adjustedDimData dat gas)
adjResGet f dat gas = f (adjustedResData dat gas)
adjElcGet f dat gas = f (adjustedElecData dat gas)

-- Dimension accessor functions
getInnerDiam = adjDimGet innerDiam
getScrewDiam = adjDimGet screwDiam
getTotalDiam = adjDimGet totalDiam
getThickness = adjDimGet thickness
getMaxDisp = adjDimGet maxDisp

-- Resonance accessor functions
getResFreq = adjResGet resFreq
getMinFreq = adjResGet minFreq
getMaxFreq = adjResGet maxFreq
getCompVol = adjResGet complianceVol
getQElec = adjResGet qElec
getQMech = adjResGet qMech
getQTotal d g = (qe * qm) / (qe + qm)
        where
        qe = getQElec d g
        qm = getQMech d g
getConeSurf d g = (pi/8) * ds * (pyth len di)
        where
        di = getInnerDiam d g
        len = getThickness d g
getCompliance d g = cv / (rho * (sos**2) * (sd**2))
        where
        cv = getCompVol d g
        rho = getRHO g
        sos = getSV g
        sd = getConeSurf d g
getBLValue d g = sqrt (rdc / (2 * pi * fres * qes * cms))
        where
        rdc = getElecResist d g
        fres = getResFreq d g
        qes = getQElec d g
        cms = getCompliance d g
getMovMassAir d g = (bl**2) * (qes / (2 * pi * fres * rdc))
        where
        bl = getBLValue d g
        qes = getQElec d g
        fres = getResFreq d g
        rdc = getElecResist d g
--getMovMassNoAir
getMechResistance d g = 2 * pi * fres * mms / qms
        where
        fres = getResFreq d g
        mms = getMovMassAir d g
        qms = getQMech d g
getSpringConstant d g = 1 / (getCompliance d g)
getSpeakerVolume d g = (5*pi/48) * len * (dt**2)                        -- Rough approximation of speaker volume
        where
        dt = getTotalDiam d g
        len = getThickness d g


getAlphaValue d g = ((f/fres)**2) - 1
