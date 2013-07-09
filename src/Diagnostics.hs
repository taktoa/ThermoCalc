module Diagnostics where

import Input
import Derived
import Utility (show', places, acc)
import Graphics.EasyPlot

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

enviroPrint a = do
    let h = "ENVIRONMENT"
    let m = [("Pressure", p),
             ("Temperature", t)]
    outputData h m
    where
    gas = getGas a
    (p, t) = (getPres gas, getTemp gas)

gaspropPrint a = do
    let h = "GAS PROPERTIES"
    let m = [("C_p", cp),
             ("C_v", cv),
             ("Gamma", gam),
             ("Prandtl #", pr),
             ("Sound speed", sos),
             ("Viscosity", mu),
             ("Conductivity", kg),
             ("Density", rho)]
    outputData h m
    where
    gas = getGas a
    (cp, cv) = (getCP gas, getCV gas)
    (gam, pr) = (getGAM gas, getPRN gas)
    (sos, mu, kg, rho) = (getSV gas, getDV gas, getTC gas, getRHO gas)

syspropPrint a = do
    let h = "SYSTEM PROPERTIES"
    let m = [("Total length", ltotal),
             ("Mach Number", mach),
             ("Abs. Loudness", aloud),
             ("Rel. Loudness", rloud),
             ("Actual COP", acop),
             ("Maximum COP", mcop),
             ("Frequency", freq),
             ("Wavelength", wl),
             ("Wavenumber", k),
             ("Temperature D", dt),
             ("Normalized TD", dtn),
             ("Pressure D", dp),
             ("Normalized PD", dpn),
             ("Thermal PD", dk),
             ("Normalized TPD", dkn),
             ("Viscous PD", dv),
             ("Normalized VPD", dvn)]
    outputData h m
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

cabinetPrint a = do
    let h = "CABINET PROPERTIES"
    let m = [("Diameter", dia),
             ("Length", len),
             ("Volume", vol)]
    outputData h m
    where
    System s i = a
    dia = speakBoxDiam (dimData i)
    (len, vol) = (getBoxLength s, getBoxVolume s)

speakerPrint a = do
    let h = "SPEAKER PROPERTIES"
    let m = [("Diameter (inner)", dinn),
             ("Diameter (screw)", dscr),
             ("Diameter (total)", dtot),
             ("Length", len),
             ("Surface area", sd),
             ("Force factor", bl),
             ("Spring constant", kc),
             ("Moving mass (WA)", mms),
             ("Moving mass (NA)", mmd),
             ("Mech. resistance", rm),
             ("Elec. resistance", re),
             ("Coil inductance", l)]
    outputData h m
    where
    System s i = a
    (dinn, dscr, dtot) = (getInnerD s i, getScrewD s i, getTotalD s i)
    (len, sd) = (getThickness s i, getConeSurf s i)
    (bl, kc) = (getBLValue s i, getSpringConstant s i)
    (mms, mmd) = (getMovMass s i, getDiaMass s i)
    (rm, re, l) = (getMechResist s i, getElecResist s i, getElecInduct s i)
    
bigTubePrint a = do
    let h = "TUBE A PROPERTIES"
    let m = [("Diameter", dia),
             ("Length", len)]
    outputData h m
    where
    (dia, len) = (getBigTubeD a, getBigTubeLength a)

heatExchangerPrint a = do
    let h = "HEX PROPERTIES"
    let m = [("Diameter", dia),
             ("Length", len),
             ("Blockage ratio", br)]
    outputData h m
    where
    (dia, len) = (getBigTubeD a, getHEXLength a)
    br = getBlockRatio (getRegenData (getInput a))

regenPrint a = do
    let h = "REGEN PROPERTIES"
    let m = [("Diameter", dia),
             ("Length", len),
             ("Blockage ratio", br),
             ("Hydraulic radius", hr)]
    outputData h m
    where
    (dia, len) = (getBigTubeD a, getHEXLength a)
    rd = getRegenData (getInput a)
    (br, hr) = (getBlockRatio rd, getHydRadius rd)

conePrint a = do
    let h = "CONE PROPERTIES"
    let m = [("Start diameter", dia1),
             ("End Diameter", dia2),
             ("Opening angle", ang),
             ("Length", len)]
    outputData h m
    where
    (dia1, dia2) = (getBigTubeD a, getSmallTubeD a)
    ang = (cang * 2) *~ degrees
    len = getConeLength a

smallTubePrint a = do
    let h = "TUBE B PROPERTIES"
    let m = [("Diameter", dia),
             ("Length", len)]
    outputData h m
    where
    (dia, len) = (getSmallTubeD a, getSmallTubeLength a)

capPrint a = do
    let h = "CAP PROPERTIES"
    let m = [("Diameter", dia),
             ("Length", len)]
    outputData h m
    where
    (dia, len) = (getBigTubeD a, getCapLength a)
