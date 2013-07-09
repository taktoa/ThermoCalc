module System where

data System = System {
                    inputData       :: InputData,
                    speakerData     :: SpeakerData
                    }

convertF :: (SpeakerData -> InputData -> a) -> (System -> a)
convertF f a = f (speakerData a) (inputData a)

convertF' :: (InputData -> SpeakerData -> a) -> (System -> a)
convertF' f a = f (inputData a) (speakerData a)

getTotalLength :: System -> Length Double
getTotalLength a = totalLength (dimData (inputData a))

getSmallTubeD :: System -> Length Double
getSmallTubeD a = smalltubeDiam (dimData (inputData a))

getBigTubeD :: System -> Length Double
getBigTubeD a = bigtubeDiam (dimData (inputData a))

getDiamRatio :: System -> Dimensionless Double
getDiamRatio a = (getSmallTubeD a) / (getBigTubeD a)

getCapLength :: System -> Length Double
getCapLength a = (getBigTubeD a) / _2

getConeLength :: System -> Length Double
getConeLength a = (d1 - d2) / (_2 * tan (dtr cang))
        where
        (d1, d2) = (getBigTubeD a, getSmallTubeD a)

getHEXLength :: System -> Length Double
getHEXLength a = (dp / (rho * sos * omg)) * sin (k * lmax)
        where
        (k, omg) = (getWavenumber a, getRotFrequency a)
        gd = gasData (inputData a)
        (rho, sos) = (getRHO gd, getSV gd)
        (dp, lmax) = (getPD a, getMaxLength a)

getRestLength :: System -> Length Double
getRestLength a = lt - ((2 * lc) + lsph)
        where
        lt = getTotalLength a
        lc = getConeLength a
        lsph = getCapLength a
        
getMaxLength :: System -> Length Double
getMaxLength a = (getRestLength a) - (getThinLength a)

getRegenLength :: System -> Length Double
getRegenLength a = 2 * ((getMaxLength a) - ((xn a) / k))
        where
        k = getWavenumber (inputData a)

getThinLength :: System -> Length Double
getThinLength a = thinLengthRoot a

getBigTubeLength :: System -> Length Double
getBigTubeLength a = (getMaxLength a) - (getRegenLength a)

getPD :: System -> Pressure Double
getNPD :: System -> Dimensionless Double

getRPD :: System -> Dimensionless Double
getRPD a = (getPD a) / (((sqrt 2) !* 20) *~ micro pascals)

getRNPD :: System -> Dimensionless Double
getRNPD a = (getNPD a) / (sqrt _2)

getALoudness :: System -> Dimensionless Double
getALoudness a = 20 * (log10 (getRPD a))

getRLoudness :: System -> Dimensionless Double
getRLoudness a = 20 * (log10 ((getRNPD a) / 20) + 11)

getMaxCOP :: System -> Dimensionless Double
getMaxCOP = copMax

getActCOP :: System -> Dimensionless Double
getActCOP = copAct

thinLengthRoot a = lrest - (bestRoot optHI (lrest/2, lrest) acc)
        where
        k = getWavenumber (inputData a)
        dr = getDiamRatio a
        (lc, lt, lrest) = (getConeLength a, getTotalLength a, getRestLength a)
        tempH lb = cot (k*(lrest - lb))
        tempI lb = (dr**2) * tan (k*(lb + lc))
        optHI lb = abs (tempH lb - tempI lb)

qcn x l a = tempA x * ((tempC * tempB x l) - tempD)
        where
        tempA x = (dkn * (dpn**2) * sin x) / (8 * gam * (1 + pr) * tempL)
        tempB x l = (dtn * tan x) / ((gam - 1) * br * l)
        tempC = (1 + pr + sqrt pr) / (1 + sqrt pr)
        tempD = 1 - (dkn * sqrt pr) + sqrt pr
        tempL = 1 - (dkn * sqrt pr) + (0.5 * pr * (dkn**2))
        br = getBlockRatio (regenData i)
        pr = getPRN (gasData i)
        gam = getGAM (gasData i)
        (dkn, dtn, dpn) = (getNTPD i, getNTD i, getNPD i)

wn x l a = (tempE x l * (tempF x l - 1)) - tempG x l
        where
        tempE x l = (dkn * l * (dpn**2) * (gam - 1) * br * cos2 x) / (4 * gam)
        tempF x l = (dtn * tan x) / (br * l * (gam - 1) * (1 + sqrt pr) * tempL)
        tempG x l = (dkn * l * (dpn**2) * sqrt pr * sin2 x) / (4 * gam * br * tempL)
        tempL = 1 - (dkn * sqrt pr) + (0.5 * pr * (dkn**2))
        i = inputData a
        br = getBlockRatio (regenData i)
        pr = getPRN (gasData i)
        gam = getGAM (gasData i)
        (dkn, dtn, dpn) = (getNTPD i, getNTD i, getNPD i)

fixedCOP x a
        | isNaN m       = 0
        | m < 0         = 0
        | m >= copMax a = 0
        | otherwise     = m
        where
        m = cop x ((4*pi*lrest/wl) - (2 * x)) a
        wl = getWavelength (inputData a)
        lrest = getMaxLength a
        cop x l a = qcn x l a / wn x l a

copMax a = (1 / (getNTD (inputData a))) - 1

copAct a = fixedCOP (xn a) a

xn a = bestRoot optX (dist, 1 - dist) acc
        where
        cm = copMax a
        optX x = (cm - fixedCOP x a) / cm
        dist = 0.1
