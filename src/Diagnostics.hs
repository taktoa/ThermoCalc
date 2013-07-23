module Diagnostics where

import Input
import Speaker
import System
import WorkingFluid
import Regenerator
import Utility
import Numeric.Units.Dimensional.Prelude
import qualified Prelude
import PrettyPrinter

-- Checks
checkMach, checkRatio, checkSpkrF, checkStack :: System -> Bool
checkTD, checkTPD, checkVPD :: System -> Bool
small = 0.25 *~ one
maxMach = 0.1 *~ one                                        -- Maximum mach number. Should not be changed, generally.
checkMach a = mach < maxMach                                -- At Mach numbers greater than maximum Mach number, equations break down.
        where
        mach = getMachNum a
checkStack a = lr*k < small                                 -- The pressure across the stack should be constant along its length
        where
        lr = getRegenLength a
        k = getWavenumber (getInput a)
checkTPD a = dkn/_2 < small                                 -- Stack spacing should be much bigger than dk
        where
        dkn = getNTPD (getInput a)
checkVPD a = dvn/_2 < small                                 -- Stack spacing should be much bigger than dv
        where
        dvn = getNVPD (getInput a)
checkTD a = dtn < small                                     -- Temp differential should be small compared to average temp
        where
        dtn = getNTD (getInput a)
checkRatio a = (hr > (_2*dk)) && (hr < (_4*dk))             -- To avoid acoustic effects, hr should be in this range
        where
        hr = getHydRadius (getRegenData (getInput a))
        dk = getTPD (getInput a)
checkSpkrF a = (f > fmin) && (f < fmax)                     -- Frequency must be within speaker's range
        where
        System i s _ = a
        f = getFrequency i
        (fmin, fmax) = (getMinFreq s i, getMaxFreq s i)

-------------------------------------------------------------------

len = 20

outputData :: (Floating a, RealFrac a, Show a, Show d) => String -> Quantity d a -> IO ()
outputData l d = putStrLn (l ++ ":" ++ replicate (len !- (1 !+ length l)) ' ' ++ (show qtr))
        where
        p = 6
        qtr = QuantityTr compatDB (round' p d)
        
separator = "--------------------------------------"

enviroPrint :: System -> IO ()
enviroPrint a = do
    putStrLn separator
    putStrLn "ENVIRONMENT:"
    outputData "Pressure"           p
    outputData "Temperature"        t
    where
    gas = getGasData (getInput a)
    (p, t) = (getPres gas, getTemp gas)

gaspropPrint :: System -> IO ()
gaspropPrint a = do
    putStrLn separator
    putStrLn "GAS PROPERTIES:"
    outputData "C_p"                cp
    outputData "C_v"                cv
    outputData "Gamma"              gam
    outputData "Prandtl #"          pr
    outputData "Sound speed"        sos
    outputData "Viscosity"          mu
    outputData "Conductivity"       kg
    outputData "Density"            rho
    where
    gas = getGasData (getInput a)
    (cp, cv) = (getCP gas, getCV gas)
    (gam, pr) = (getGAM gas, getPRN gas)
    (sos, mu, kg, rho) = (getSV gas, getDV gas, getTC gas, getRHO gas)

syspropPrint :: System -> IO ()
syspropPrint a = do
    putStrLn separator
    putStrLn "SYSTEM PROPERTIES:"
    outputData "Total length"       ltotal
    outputData "Resonator length"   lres
    outputData "Mach number"        mach
    outputData "Abs. loudness"      aloud
    outputData "Rel. loudness"      rloud
    outputData "Actual COP"         acop
    outputData "Maximum COP"        mcop
    outputData "Frequency"          f
    outputData "Wavelength"         wl
    outputData "Wavenumber"         k
    outputData "Temperature D"      dt
    outputData "Normalized TD"      dtn
    outputData "Pressure D"         dp
    outputData "Normalized PD"      dpn
    outputData "Thermal PD"         dk
    outputData "Normalized TPD"     dkn
    outputData "Viscous PD"         dv
    outputData "Normalized VPD"     dvn
    where
    i = getInput a
    (ltotal, lres) = (getCompLength a, getTotalLength a)
    (mach, aloud, rloud) = (getMachNum a, getALoudness a, getRLoudness a)
    (acop, mcop) = (getActCOP a, getMaxCOP a)
    (f, wl, k) = (getFrequency i, getWavelength i, getWavenumber i)
    (dt, dtn, dp, dpn) = (getTD i, getNTD i, getPD a, getNPD a)
    (dk, dkn, dv, dvn) = (getTPD i, getNTPD i, getVPD i, getNVPD i)

diagChecks :: System -> IO ()
diagChecks a = do
    putStrLn "-------------------"
    putStrLn "DIAGNOSTICS:"
    putStrLn (if checkMach a    then "Mach number check passed."    else "Mach number too high!")
    putStrLn (if checkTPD a     then "TPD check passed."            else "TPD check failed!")
    putStrLn (if checkVPD a     then "VPD check passed."            else "VPD check failed!")
    putStrLn (if checkTD a      then "TD check passed."             else "TD check failed!")
    putStrLn (if checkStack a   then "Stack check passed."          else "Stack check failed!")
    putStrLn (if checkRatio a   then "Ratio check passed."          else "Ratio check failed!")
    putStrLn (if checkSpkrF a   then "Speaker freq check passed."   else "Speaker freq check failed!")
    putStrLn ""

cabinetPrint :: System -> IO ()
cabinetPrint a = do
    putStrLn separator
    putStrLn "CABINET PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    outputData "Alpha"              alpha
    outputData "Volume"             vol
    where
    System i s _ = a
    dia = speakBoxDiam (dimData i)
    (len, vol) = (getBoxLength s i, getBoxVolume s i)
    alpha = getAlphaValue s i

speakerPrint :: System -> IO ()
speakerPrint a = do
    putStrLn separator
    putStrLn "SPEAKER PROPERTIES:"
    outputData "Diameter (inner)"   dinn
    outputData "Diameter (screw)"   dscr
    outputData "Diameter (total)"   dtot
    outputData "Length"             len
    outputData "Surface area"       sd
    outputData "Force factor"       bl
    outputData "Spring constant"    kc
    outputData "Moving mass (WA)"   mms
    outputData "Moving mass (NA)"   mmd
    outputData "Mech. resistance"   rm
    outputData "Elec. resistance"   re
    outputData "Coil inductance"    l
    where
    System i s _ = a
    (dinn, dscr, dtot) = (getInnerD s i, getScrewD s i, getTotalD s i)
    (len, sd) = (getThickness s i, getConeSurf s i)
    (bl, kc) = (getBLValue s i, getSpringConstant s i)
    (mms, mmd) = (getMovMass s i, getDiaMass s i)
    (rm, re, l) = (getMechResist s i, getElecResist s i, getElecInduct s i)

bigTubePrint :: System -> IO ()
bigTubePrint a = do
    putStrLn separator
    putStrLn "TUBE A PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    where
    (dia, len) = (getBigTubeD a, getBigTubeLength a)

heatExchangerPrint :: System -> IO ()
heatExchangerPrint a = do
    putStrLn separator
    putStrLn "HEX PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    outputData "Blockage ratio"     br
    where
    (dia, len) = (getBigTubeD a, getHEXLength a)
    br = getBlockRatio (getRegenData (getInput a))

regenPrint :: System -> IO ()
regenPrint a = do
    putStrLn separator
    putStrLn "REGEN PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    outputData "Blockage ratio"     br
    outputData "Hydraulic radius"   hr
    where
    (dia, len) = (getBigTubeD a, getRegenLength a)
    rd = getRegenData (getInput a)
    (br, hr) = (getBlockRatio rd, getHydRadius rd)

conePrint :: System -> IO ()
conePrint a = do
    putStrLn separator
    putStrLn "CONE PROPERTIES:"
    outputData "Start diameter"     dia1
    outputData "End diameter"       dia2
    outputData "Opening angle"      ang
    outputData "Length"             len
    where
    (dia1, dia2) = (getBigTubeD a, getSmallTubeD a)
    ang = (cang !* 2) *~ degree
    len = getConeLength a

smallTubePrint :: System -> IO ()
smallTubePrint a = do
    putStrLn separator
    putStrLn "TUBE B PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    where
    (dia, len) = (getSmallTubeD a, getSmallTubeLength a)

capPrint :: System -> IO ()
capPrint a = do
    putStrLn separator
    putStrLn "CAP PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    where
    (dia, len) = (getBigTubeD a, getCapLength a)
