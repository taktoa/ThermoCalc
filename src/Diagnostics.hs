module Diagnostics where

import Input
import Speaker
import System
import WorkingFluid
import Regenerator
import Utility
import Numeric.Units.Dimensional.Prelude
import qualified Prelude

-- Checks
--small = 0.25
--maxMach = 0.1                                               -- Maximum mach number. Should not be changed, generally.
--checkMach = mach < maxMach                                  -- At Mach numbers greater than maximum Mach number, equations break down.
--checkStack = lr*k < small                                   -- The pressure across the stack should be constant along its length
--checkTPD = dkn/2 < small                                    -- Stack spacing should be much bigger than dk
--checkVPD = dvn/2 < small                                    -- Stack spacing should be much bigger than dv
--checkTD = dtn < small                                       -- Temp differential should be small compared to average temp
--checkRatio = (hr > (2*dk)) && (hr < (4*dk))                 -- To avoid acoustic effects, hr should be in this range
--checkSpkrF = (f > spfmin) && (f < spfmax)                   -- Frequency must be within speaker's range
--checkSpkrD = spdtotal < d0                                  -- Frequency must be within speaker's range

-------------------------------------------------------------------

len = 20

outputData :: (Num a, Show a, Show d) => String -> Quantity d a -> IO ()
outputData l d = putStrLn (l ++ ":" ++ (replicate (len !- (1 !+ length l)) ' ') ++ show d)

enviroPrint :: System -> IO ()
enviroPrint a = do
    putStrLn "--------------------------------------"
    putStrLn "ENVIRONMENT:"
    outputData "Pressure"           p
    outputData "Temperature"        t
    where
    gas = getGasData (getInput a)
    (p, t) = (getPres gas, getTemp gas)

gaspropPrint :: System -> IO ()
gaspropPrint a = do
    putStrLn "--------------------------------------"
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
    putStrLn "--------------------------------------"
    putStrLn "SYSTEM PROPERTIES:"
    outputData "Total length"       ltotal
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
    ltotal = getCompLength a
    (mach, aloud, rloud) = (getMachNum a, getALoudness a, getRLoudness a)
    (acop, mcop) = (getActCOP a, getMaxCOP a)
    (f, wl, k) = (getFrequency i, getWavelength i, getWavenumber i)
    (dt, dtn, dp, dpn) = (getTD i, getNTD i, getPD a, getNPD a)
    (dk, dkn, dv, dvn) = (getTPD i, getNTPD i, getVPD i, getNVPD i)

--diagChecks = do
    --putStrLn "-------------------"
    --putStrLn "DIAGNOSTICS:"
    --putStrLn (if checkMach  then "Mach number check passed."    else "Mach number too high! "      ++ show' mach     ++ " > " ++ show' maxMach)
    --putStrLn (if checkTPD   then "TPD check passed."            else "TPD check failed! "          ++ show' (dkn/2)  ++ " > " ++ show' small)
    --putStrLn (if checkVPD   then "VPD check passed."            else "VPD check failed! "          ++ show' (dvn/2)  ++ " > " ++ show' small)
    --putStrLn (if checkTD    then "TD check passed."             else "TD check failed! "           ++ show' dtn      ++ " > " ++ show' small)
    --putStrLn (if checkStack then "Stack check passed."          else "Stack check failed! "        ++ show' (lr*k)   ++ " > " ++ show' small)
    --putStrLn (if checkRatio then "Ratio check passed."          else "Ratio check failed! "        ++ show' (hr/dk)  ++ " | " ++ show' 2.0000 ++ ", " ++ show' 4.0000)
    --putStrLn (if checkSpkrF then "Speaker freq check passed."   else "Speaker freq check failed! " ++ show' f        ++ " | " ++ show' spfmin ++ ", " ++ show' spfmax)
    --putStrLn (if checkSpkrD then "Speaker diam check passed."   else "Speaker diam check failed! " ++ show' spdtotal ++ " > " ++ show' d0)
    --putStrLn ""

cabinetPrint :: System -> IO ()
cabinetPrint a = do
    putStrLn "--------------------------------------"
    putStrLn "CABINET PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    outputData "Volume"             vol
    where
    System i s = a
    dia = speakBoxDiam (dimData i)
    (len, vol) = (getBoxLength s i, getBoxVolume s i)

speakerPrint :: System -> IO ()
speakerPrint a = do
    putStrLn "--------------------------------------"
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
    System i s = a
    (dinn, dscr, dtot) = (getInnerD s i, getScrewD s i, getTotalD s i)
    (len, sd) = (getThickness s i, getConeSurf s i)
    (bl, kc) = (getBLValue s i, getSpringConstant s i)
    (mms, mmd) = (getMovMass s i, getDiaMass s i)
    (rm, re, l) = (getMechResist s i, getElecResist s i, getElecInduct s i)

bigTubePrint :: System -> IO ()
bigTubePrint a = do
    putStrLn "--------------------------------------"
    putStrLn "TUBE A PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    where
    (dia, len) = (getBigTubeD a, getBigTubeLength a)

heatExchangerPrint :: System -> IO ()
heatExchangerPrint a = do
    putStrLn "--------------------------------------"
    putStrLn "HEX PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    outputData "Blockage ratio"     br
    where
    (dia, len) = (getBigTubeD a, getHEXLength a)
    br = getBlockRatio (getRegenData (getInput a))

regenPrint :: System -> IO ()
regenPrint a = do
    putStrLn "--------------------------------------"
    putStrLn "REGEN PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    outputData "Blockage ratio"     br
    outputData "Hydraulic radius"   hr
    where
    (dia, len) = (getBigTubeD a, getHEXLength a)
    rd = getRegenData (getInput a)
    (br, hr) = (getBlockRatio rd, getHydRadius rd)

conePrint :: System -> IO ()
conePrint a = do
    putStrLn "--------------------------------------"
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
    putStrLn "--------------------------------------"
    putStrLn "TUBE B PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    where
    (dia, len) = (getSmallTubeD a, getSmallTubeLength a)

capPrint :: System -> IO ()
capPrint a = do
    putStrLn "--------------------------------------"
    putStrLn "CAP PROPERTIES:"
    outputData "Diameter"           dia
    outputData "Length"             len
    where
    (dia, len) = (getBigTubeD a, getCapLength a)
