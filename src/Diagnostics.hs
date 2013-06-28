module Diagnostics where

import Input
import Derived
import Utility (show', places, acc)
import Graphics.EasyPlot
import Display (displayDiag)

-- Checks
small = 0.25
maxMach = 0.1                                               -- Maximum mach number. Should not be changed, generally.
checkMach = mach < maxMach                                  -- At Mach numbers greater than maximum Mach number, equations break down.
checkStack = lr*k < small                                   -- The pressure across the stack should be constant along its length
checkTPD = dkn/2 < small                                    -- Stack spacing should be much bigger than dk
checkVPD = dvn/2 < small                                    -- Stack spacing should be much bigger than dv
checkTD = dtn < small                                       -- Temp differential should be small compared to average temp
checkRatio = (hr > (2*dk)) && (hr < (4*dk))                 -- To avoid acoustic effects, hr should be in this range
checkSpkr = (f > spfmin) && (f < spfmax)                    -- Frequency must be within speaker's range


-------------------------------------------------------------------

enviroPrint = do
    putStrLn "-------------------"
    putStrLn "ENVIRONMENT:"
    putStrLn ("Pressure:            " ++ show' p        ++ " bar")
    putStrLn ("Temp:                " ++ show' t        ++ " K")
    putStrLn ""

gaspropPrint = do
    putStrLn "-------------------"
    putStrLn "GAS PROPERTIES:"
    putStrLn ("C_p:                 " ++ show' cp       ++ " J/(g*K)")
    putStrLn ("C_v:                 " ++ show' cv       ++ " J/(g*K)")
    putStrLn ("Gamma:               " ++ show' gam      ++ "")
    putStrLn ("Prandtl Number:      " ++ show' pr       ++ "")
    putStrLn ("SoS:                 " ++ show' sos      ++ " mm/s")
    putStrLn ("Viscosity:           " ++ show' mu       ++ " cP")
    putStrLn ("Conductivity:        " ++ show' kg       ++ " W/(m*K)")
    putStrLn ("Density:             " ++ show' rho      ++ " g/mL")
    putStrLn ""

syspropPrint = do
    putStrLn "-------------------"
    putStrLn "SYSTEM PROPERTIES:"
    putStrLn ("Mach #:              " ++ show' mach     ++ " Mach")
    putStrLn ("Actual COP:          " ++ show' copAct   ++ "")
    putStrLn ("Maximum COP:         " ++ show' copMax   ++ "")
    putStrLn ("Frequency:           " ++ show' f        ++ " Hz")
    putStrLn ("Wavelength:          " ++ show' wl       ++ " mm")
    putStrLn ("Normalizer:          " ++ show' k        ++ " mm^-1")
    putStrLn ("Temp. diff.:         " ++ show' dt       ++ " K")
    putStrLn ("Norm. temp. diff.:   " ++ show' dtn      ++ "")
    putStrLn ("Pres. diff.:         " ++ show' dp       ++ " bar")
    putStrLn ("Norm. pres. diff.:   " ++ show' dpn      ++ "")
    putStrLn ("Thermal PD:          " ++ show' dk       ++ " mm")
    putStrLn ("Normalized TPD:      " ++ show' dkn      ++ "")
    putStrLn ("Viscous PD:          " ++ show' dv       ++ " mm")
    putStrLn ("Normalized VPD:      " ++ show' dvn      ++ "")
    putStrLn ""

diagChecks = do
    putStrLn "-------------------"
    putStrLn "DIAGNOSTICS:"
    putStrLn (if checkMach  then "Mach number check passed."    else "Mach number too high! "   ++ show' mach)
    putStrLn (if checkTPD   then "TPD check passed."            else "TPD check failed! "       ++ show' (dkn/2))
    putStrLn (if checkVPD   then "VPD check passed."            else "VPD check failed! "       ++ show' (dvn/2))
    putStrLn (if checkTD    then "TD check passed."             else "TD check failed! "        ++ show' dtn)
    putStrLn (if checkStack then "Stack check passed."          else "Stack check failed! "     ++ show' (lr*k))
    putStrLn (if checkRatio then "Ratio check passed."          else "Ratio check failed! "     ++ show' (hr/dk))
    putStrLn (if checkSpkr  then "Speaker check passed."        else "Speaker check failed! "   ++ show' (f))
    putStrLn ""

speakerPrint = do
    putStrLn "-------------------"
    putStrLn "SPEAKER PROPERTIES:"
    putStrLn ("Diameter (inner):    " ++ show' spdsmall ++ " mm")
    putStrLn ("Diameter (screw):    " ++ show' spdscrew ++ " mm")
    putStrLn ("Diameter (total):    " ++ show' spdtotal ++ " mm")
    putStrLn ("Length:              " ++ show' splen    ++ " mm")
    putStrLn ("P. surface area:     " ++ show' spsd     ++ " mm^2")
    putStrLn ("Force factor (Bl):   " ++ show' spbl     ++ " T*m")
    putStrLn ("Spring constant:     " ++ show' spkc     ++ " N/m")
    putStrLn ("Moving mass:         " ++ show' spmms    ++ " kg")
    putStrLn ("Mech. resistance:    " ++ show' sprms    ++ " N*s/m")
    putStrLn ("Elec. resistance:    " ++ show' sprdc    ++ " ohm")
    putStrLn ("Coil inductance:     " ++ show' spinduc  ++ " H")
    putStrLn ""

bigTubePrint = do
    putStrLn "-------------------"
    putStrLn "TUBE A PROPERTIES:"
    putStrLn ("Diameter:            " ++ show' d1       ++ " mm")
    putStrLn ("Length:              " ++ show' lta      ++ " mm")
    putStrLn ("Cross-section area:  " ++ show' xa1      ++ " mm^2")
    putStrLn ("Volume:              " ++ show' vta      ++ " mm^3")
    putStrLn ""

heatExchangerPrint = do
    putStrLn "-------------------"
    putStrLn "HEX PROPERTIES:"
    putStrLn ("Diameter:            " ++ show' d1       ++ " mm")
    putStrLn ("Length:              " ++ show' lhex     ++ " mm")
    putStrLn ("Blockage ratio:      " ++ show' br       ++ "")
    putStrLn ("Cross-section area:  " ++ show' xa1      ++ " mm^2")
    putStrLn ("Volume:              " ++ show' vhex     ++ " mm^3")
    putStrLn ""

regenPrint = do
    putStrLn "-------------------"
    putStrLn "REGEN PROPERTIES:"
    putStrLn ("Diameter:            " ++ show' d1       ++ " mm")
    putStrLn ("Length:              " ++ show' lr       ++ " mm")
    putStrLn ("Blockage ratio:      " ++ show' br       ++ "")
    putStrLn ("Hydraulic radius:    " ++ show' hr       ++ " mm")
    putStrLn ("Stack spacing:       " ++ show' (hr/2)   ++ " mm")
    putStrLn ("Plate thickness:     " ++ show' st       ++ " mm")
    putStrLn ("Cross-section area:  " ++ show' xa1      ++ " mm^2")
    putStrLn ("Volume:              " ++ show' vr       ++ " mm^3")
    putStrLn ""

conePrint = do
    putStrLn "-------------------"
    putStrLn "CONE PROPERTIES:"
    putStrLn ("Start Diameter:      " ++ show' d1       ++ " mm")
    putStrLn ("End Diameter:        " ++ show' d2       ++ " mm")
    putStrLn ("Length:              " ++ show' lc       ++ " mm")
    putStrLn ("Opening Angle:       " ++ show' (2*ang)  ++ " deg")
    putStrLn ("Volume:              " ++ show' vc       ++ " mm^3")
    putStrLn ""

smallTubePrint = do
    putStrLn "-------------------"
    putStrLn "TUBE B PROPERTIES:"
    putStrLn ("Diameter:            " ++ show' d2       ++ " mm")
    putStrLn ("Length:              " ++ show' lb       ++ " mm")
    putStrLn ("Cross-section area:  " ++ show' xa2      ++ " mm^2")
    putStrLn ("Volume:              " ++ show' vb       ++ " mm^3")
    putStrLn ""

capPrint = do
    putStrLn "-------------------"
    putStrLn "CAP PROPERTIES:"
    putStrLn ("Diameter:            " ++ show' d1       ++ " mm")
    putStrLn ("Length:              " ++ show' lsph     ++ " mm")
    putStrLn ("Volume:              " ++ show' vsph     ++ " mm^3")
    putStrLn ""


diagnostic = do
    enviroPrint
    gaspropPrint
    syspropPrint
    speakerPrint
    bigTubePrint
    heatExchangerPrint
    regenPrint
    smallTubePrint
    conePrint
    capPrint
    diagChecks
    displayDiag
--    let options = [Title "COP vs X"]
--    let options2D = [Range 0 1, Step acc]
--    let func = Function2D options options2D (optX)
--    plot' [Interactive] X11 func

