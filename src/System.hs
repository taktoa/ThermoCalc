module System where

import Speaker
import Input
import WorkingFluid
import Regenerator
import Utility
import Numeric.Units.Dimensional.Prelude
import qualified Prelude

data System = System {
                    inputData       :: InputData,
                    speakerData     :: SpeakerData
                    }

getInput :: System -> InputData
getInput = inputData

getSpeaker :: System -> SpeakerData
getSpeaker = speakerData

cF :: (SpeakerData -> InputData -> a) -> System -> a
cF f a = f (speakerData a) (inputData a)

cF' :: (InputData -> SpeakerData -> a) -> System -> a
cF' f a = f (inputData a) (speakerData a)

getTotalLength :: System -> Length Double
getTotalLength a = totalLength (dimData (getInput a))

getCompLength :: System -> Length Double
getCompLength a = getTotalLength a + cF getBoxLength a

getSmallTubeD :: System -> Length Double
getSmallTubeD a = smalltubeDiam (dimData (getInput a))

getBigTubeD :: System -> Length Double
getBigTubeD a = bigtubeDiam (dimData (getInput a))

getDiamRatio :: System -> DimlessDouble
getDiamRatio a = getSmallTubeD a / getBigTubeD a

getCapLength :: System -> Length Double
getCapLength a = getBigTubeD a / _2

getConeLength :: System -> Length Double
getConeLength a = (d1 - d2) / (_2 * tan (dtr cang *~ one))
        where
        (d1, d2) = (getBigTubeD a, getSmallTubeD a)

getHEXLength :: System -> Length Double
getHEXLength a = (dp / (rho * sos * omg)) * sin (k * lmax)
        where
        System i s = a
        (k, omg) = (getWavenumber i, getRotFrequency i)
        gd = gasData (getInput a)
        (rho, sos) = (getRHO gd, getSV gd)
        (dp, lmax) = (getPD a, getMaxLength a)

getRestLength :: System -> Length Double
getRestLength a = lt - ((_2 * lc) + lsph)
        where
        lt = getTotalLength a
        lc = getConeLength a
        lsph = getCapLength a
        
getMaxLength :: System -> Length Double
getMaxLength a = getRestLength a - getSmallTubeLength a

getRegenLength :: System -> Length Double
getRegenLength a = _2 * (getMaxLength a - (xn a / k))
        where
        k = getWavenumber (getInput a)

getSmallTubeLength :: System -> Length Double
getSmallTubeLength = thinLengthRoot

getBigTubeLength :: System -> Length Double
getBigTubeLength a = getMaxLength a - getRegenLength a

getPD :: System -> Pressure Double
getPD a = 3000 *~ pascal

getNPD :: System -> DimlessDouble
getNPD a = getPD a / getPres (getGasData (getInput a))

getRPD :: System -> DimlessDouble
getRPD a = getPD a / ((20 !* psqrt 2.0) *~ micro pascal)

getRNPD :: System -> DimlessDouble
getRNPD a = getRPD a * atmoPres / getPres (getGasData (getInput a))

getALoudness :: System -> DimlessDouble
getALoudness a = (20 *~ one) * log10' (getRPD a)

getRLoudness :: System -> DimlessDouble
getRLoudness a = (20 *~ one) * log10' (getRNPD a)

getMachNum :: System -> DimlessDouble
getMachNum a = getPD a / (rho * squ sos)
        where
        gd = getGasData (getInput a)
        (rho, sos) = (getRHO gd, getSV gd)

getMaxCOP :: System -> DimlessDouble
getMaxCOP = copMax

getActCOP :: System -> DimlessDouble
getActCOP = copAct

thinLengthRoot :: System -> Length Double
thinLengthRoot a = lrest - (lt * bestRoot' optHI brckt accD)
        where
        brckt = (lrest/(_2*lt), lrest/lt)
        k = getWavenumber (getInput a)
        dr = getDiamRatio a
        (lc, lt, lrest) = (getConeLength a, getTotalLength a, getRestLength a)
        tempH lb = _1 / tan (k*(lrest - (lb*lt)))
        tempI lb = (dr**_2) * tan (k*((lb*lt) + lc))
        optHI lb = abs (tempH lb - tempI lb)

qcn :: DimlessDouble -> DimlessDouble -> System -> DimlessDouble
qcn x l a = tempA x * ((tempC * tempB x l) - tempD)
        where
        tempA x = (dkn * (dpn**_2) * sin x) / (_4 * gam * (_1 + pr) * tempL)
        tempB x l = (dtn * tan x) / ((gam - _1) * br * l)
        tempC = (_1 + pr + sqrt pr) / (_1 + sqrt pr)
        tempD = _1 - (dkn * sqrt pr) + sqrt pr
        tempL = _2 - (_2 * dkn * sqrt pr) + (pr * (dkn**_2))
        i = getInput a
        br = getBlockRatio (regenData i)
        pr = getPRN (gasData i)
        gam = getGAM (gasData i)
        (dkn, dtn, dpn) = (getNTPD i, getNTD i, getNPD a)

wn :: DimlessDouble -> DimlessDouble -> System -> DimlessDouble
wn x l a = (tempE x l * (tempF x l - _1) / (_4 * gam)) - tempG x l
        where
        tempE x l = dkn * l * (dpn**_2) * (gam - _1) * br * (cos x)**_2
        tempF x l = (_2 * dtn * tan x) / (br * l * (gam - _1) * (_1 + sqrt pr) * tempL)
        tempG x l = (dkn * l * (dpn**_2) * sqrt pr * (sin x)**_2) / (_4 * gam * br * tempL)
        tempL = _2 - (_2 * dkn * sqrt pr) + (pr * (dkn**_2))
        i = getInput a
        br = getBlockRatio (regenData i)
        pr = getPRN (gasData i)
        gam = getGAM (gasData i)
        (dkn, dtn, dpn) = (getNTPD i, getNTD i, getNPD a)

fixedCOP :: DimlessDouble -> System -> DimlessDouble
fixedCOP x a
        | isNaN' m      = _0
        | m < _0        = _0
        | m >= copMax a = _0
        | otherwise     = m
        where
        m = cop x ((_4*pi*lrest/wl) - (_2 * x)) a
        wl = getWavelength (getInput a)
        lrest = getMaxLength a
        cop x l a = qcn x l a / wn x l a

copMax :: System -> DimlessDouble
copMax a = (_1 / getNTD (getInput a)) - _1

copAct :: System -> DimlessDouble
copAct a = fixedCOP (xn a) a

xn :: System -> DimlessDouble
xn a = bestRoot' optX (dist, _1 - dist) accD
        where
        cm = copMax a
        optX x = (cm - fixedCOP x a) / cm
        dist = 0.1 *~ one
