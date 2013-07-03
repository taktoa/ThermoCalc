module Diagnostics where

import Input
import Derived
import Utility (show', places, acc)
import Graphics.EasyPlot

-- Checks
small = 0.25
maxMach = 0.1                                               -- Maximum mach number. Should not be changed, generally.
checkMach = mach < maxMach                                  -- At Mach numbers greater than maximum Mach number, equations break down.
checkStack = lr*k < small                                   -- The pressure across the stack should be constant along its length
checkTPD = dkn/2 < small                                    -- Stack spacing should be much bigger than dk
checkVPD = dvn/2 < small                                    -- Stack spacing should be much bigger than dv
checkTD = dtn < small                                       -- Temp differential should be small compared to average temp
checkRatio = (hr > (2*dk)) && (hr < (4*dk))                 -- To avoid acoustic effects, hr should be in this range
checkSpkrF = (f > spfmin) && (f < spfmax)                   -- Frequency must be within speaker's range
checkSpkrD = spdtotal < d0                                  -- Frequency must be within speaker's range

-------------------------------------------------------------------

printNum lbl num units = putStrLn (lbl ++ show' num ++ units)

enviroPrint = do
    putStrLn "-------------------"
    putStrLn "ENVIRONMENT:"
    printNum "Pressure:            "   p        " bar"
    printNum "Temp:                "   t        " K"
    putStrLn ""

gaspropPrint = do
    putStrLn "-------------------"
    putStrLn "GAS PROPERTIES:"
    printNum "C_p:                 "   cp       " J/(g*K)"
    printNum "C_v:                 "   cv       " J/(g*K)"
    printNum "Gamma:               "   gam      ""
    printNum "Prandtl Number:      "   pr       ""
    printNum "SoS:                 "   sos      " mm/s"
    printNum "Viscosity:           "   mu       " cP"
    printNum "Conductivity:        "   kg       " W/(m*K)"
    printNum "Density:             "   rho      " g/mL"
    putStrLn ""

syspropPrint = do
    putStrLn "-------------------"
    putStrLn "SYSTEM PROPERTIES:"
    printNum "Total length:        "   ltotal   " mm"
    printNum "Mach #:              "   mach     " Mach"
    printNum "Loudness:            "   loud     " dB SPL"
    printNum "Actual COP:          "   copAct   ""
    printNum "Maximum COP:         "   copMax   ""
    printNum "Frequency:           "   f        " Hz"
    printNum "Wavelength:          "   wl       " mm"
    printNum "Normalizer:          "   k        " mm^-1"
    printNum "Temp. diff.:         "   dt       " K"
    printNum "Norm. temp. diff.:   "   dtn      ""
    printNum "Pres. diff.:         "   dp       " bar"
    printNum "Norm. pres. diff.:   "   dpn      ""
    printNum "Thermal PD:          "   dk       " mm"
    printNum "Normalized TPD:      "   dkn      ""
    printNum "Viscous PD:          "   dv       " mm"
    printNum "Normalized VPD:      "   dvn      ""
    putStrLn ""

diagChecks = do
    putStrLn "-------------------"
    putStrLn "DIAGNOSTICS:"
    putStrLn (if checkMach  then "Mach number check passed."    else "Mach number too high! "      ++ show' mach     ++ " > " ++ show' maxMach)
    putStrLn (if checkTPD   then "TPD check passed."            else "TPD check failed! "          ++ show' (dkn/2)  ++ " > " ++ show' small)
    putStrLn (if checkVPD   then "VPD check passed."            else "VPD check failed! "          ++ show' (dvn/2)  ++ " > " ++ show' small)
    putStrLn (if checkTD    then "TD check passed."             else "TD check failed! "           ++ show' dtn      ++ " > " ++ show' small)
    putStrLn (if checkStack then "Stack check passed."          else "Stack check failed! "        ++ show' (lr*k)   ++ " > " ++ show' small)
    putStrLn (if checkRatio then "Ratio check passed."          else "Ratio check failed! "        ++ show' (hr/dk)  ++ " | " ++ show' 2.0000 ++ ", " ++ show' 4.0000)
    putStrLn (if checkSpkrF then "Speaker freq check passed."   else "Speaker freq check failed! " ++ show' f        ++ " | " ++ show' spfmin ++ ", " ++ show' spfmax)
    putStrLn (if checkSpkrD then "Speaker diam check passed."   else "Speaker diam check failed! " ++ show' spdtotal ++ " > " ++ show' d0)
    putStrLn ""

cabinetPrint = do
    putStrLn "-------------------"
    putStrLn "CABINET PROPERTIES:"
    printNum "Diameter:            "   d0       " mm"
    printNum "Length:              "   lbox     " mm"
    printNum "Cross-section area:  "   xa0      " mm^2"
    printNum "Volume:              "   vbox     " mm^3"
    putStrLn ""


speakerPrint = do
    putStrLn "-------------------"
    putStrLn "SPEAKER PROPERTIES:"
    printNum "Diameter (inner):    "   spdsmall " mm"
    printNum "Diameter (screw):    "   spdscrew " mm"
    printNum "Diameter (total):    "   spdtotal " mm"
    printNum "Length:              "   splen    " mm"
    printNum "P. surface area:     "   spsd     " mm^2"
    printNum "Force factor (Bl):   "   spbl     " T*m"
    printNum "Spring constant:     "   spkc     " N/m"
    printNum "Moving mass:         "   spmms    " kg"
    printNum "Mech. resistance:    "   sprms    " N*s/m"
    printNum "Elec. resistance:    "   sprdc    " ohm"
    printNum "Coil inductance:     "   spinduc  " H"
    putStrLn ""

bigTubePrint = do
    putStrLn "-------------------"
    putStrLn "TUBE A PROPERTIES:"
    printNum "Diameter:            "   d1       " mm"
    printNum "Length:              "   lta      " mm"
    printNum "Cross-section area:  "   xa1      " mm^2"
    printNum "Volume:              "   vta      " mm^3"
    putStrLn ""

heatExchangerPrint = do
    putStrLn "-------------------"
    putStrLn "HEX PROPERTIES:"
    printNum "Diameter:            "   d1       " mm"
    printNum "Length:              "   lhex     " mm"
    printNum "Blockage ratio:      "   br       ""
    printNum "Cross-section area:  "   xa1      " mm^2"
    printNum "Volume:              "   vhex     " mm^3"
    putStrLn ""

regenPrint = do
    putStrLn "-------------------"
    putStrLn "REGEN PROPERTIES:"
    printNum "Diameter:            "   d1       " mm"
    printNum "Length:              "   lr       " mm"
    printNum "Blockage ratio:      "   br       ""
    printNum "Hydraulic radius:    "   hr       " mm"
    printNum "Stack spacing:       "   (hr/2)   " mm"
    printNum "Plate thickness:     "   st       " mm"
    printNum "Cross-section area:  "   xa1      " mm^2"
    printNum "Volume:              "   vr       " mm^3"
    putStrLn ""

conePrint = do
    putStrLn "-------------------"
    putStrLn "CONE PROPERTIES:"
    printNum "Start Diameter:      "   d1       " mm"
    printNum "End Diameter:        "   d2       " mm"
    printNum "Length:              "   lc       " mm"
    printNum "Opening Angle:       "   (2*cang) " deg"
    printNum "Volume:              "   vc       " mm^3"
    putStrLn ""

smallTubePrint = do
    putStrLn "-------------------"
    putStrLn "TUBE B PROPERTIES:"
    printNum "Diameter:            "   d2       " mm"
    printNum "Length:              "   lb       " mm"
    printNum "Cross-section area:  "   xa2      " mm^2"
    printNum "Volume:              "   vb       " mm^3"
    putStrLn ""

capPrint = do
    putStrLn "-------------------"
    putStrLn "CAP PROPERTIES:"
    printNum "Diameter:            "   d1       " mm"
    printNum "Length:              "   lsph     " mm"
    printNum "Volume:              "   vsph     " mm^3"
    putStrLn ""
