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
getRotResFreq d g = (2 * pi * radian) * getResFreq d g


spomgres = 2 * pi * spfres              -- rad/s            -- Rotational resonant frequency of speaker
spsd = pi * sprsmall * l                -- mm^2             -- Speaker cone projected area
    where l = sqrt (((splen/2)**2) + (sprsmall**2))
spcms = (10**6) * spvas / a             -- m/N              -- Speaker compliance
    where a = rho * (sos**2) * (spsd**2)
spbl = sqrt (sprdc / a)                 -- T * m            -- Speaker force factor
    where a = spomgres * spqes * spcms
spmms = (spbl**2) * a                   -- kg               -- Speaker moving mass, including air
    where a = spqes / (spomgres * sprdc)
sprms = spomgres * spmms / spqms        -- N*s/m            -- Speaker mechanical resistance
spkc = 1 / spcms                        -- N/m              -- Speaker spring constant
spalpha = ((f/spfres)**2) - 1           -- DL               -- Speaker cabinet alpha
vmagnet = pi*splen*(spdtotal**2.0)/32   -- mm^3             -- Rough approximation of speaker magnet volume
vscone = 7*pi*splen*(spdtotal**2.0)/96  -- mm^3             -- Rough approximation of speaker cone volume
vspeaker = vmagnet + vscone             -- mm^3             -- Rough approximation of speaker volume
