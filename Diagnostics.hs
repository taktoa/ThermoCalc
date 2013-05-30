module Diagnostics where

import Input
import Derived
import Utility (show', places)
import Graphics.EasyPlot

-- Checks
small = 0.05
maxMach = 0.1
checkMach = mach < maxMach                  -- At Mach numbers greater than 0.1, equations break down
--checkStack = Ls < (small/k)               -- The pressure across the stack should be constant along its length
checkTPD = 2*hr*small > dk                  -- Stack spacing should be much bigger than dk
checkVPD = 2*hr*small > dv                  -- Stack spacing should be much bigger than dv
checkTD = t*small > dt                      -- Temp differential should be small compared to average temp
checkStack = (hr > (2*dk)) && (hr < (4*dk)) -- To avoid acoustic effects, hr should be in this range

-------------------------------------------------------------------

enviroPrint = do
    putStrLn "------------------"
    putStrLn "ENVIRONMENT:"
    putStrLn ("Pressure:            " ++ show' p        ++ " bar")
    putStrLn ("Temp:                " ++ show' t        ++ " K")
    putStrLn ""

gaspropPrint = do
    putStrLn "------------------"
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

dimensionsPrint = do
    putStrLn "------------------"
    putStrLn "DIMENSIONS:"
    putStrLn ("Diameter #1:         " ++ show' d1       ++ " mm")
    putStrLn ("Diameter #2:         " ++ show' d2       ++ " mm")
    putStrLn ("Total length:        " ++ show' lt       ++ " mm")
    putStrLn ("Thin tube length:    " ++ show' lb       ++ " mm")
    putStrLn ("Cap length:          " ++ show' lsph     ++ " mm")
    putStrLn ("HEX length:          " ++ show' lhex     ++ " mm")
    putStrLn ("Cone length:         " ++ show' lc       ++ " mm")
    putStrLn ("Stack spacing:       " ++ show' (hr/2)   ++ " mm")
    putStrLn ("X-section area #1:   " ++ show' xa1      ++ " mm^2")
    putStrLn ("X-section area #2:   " ++ show' xa2      ++ " mm^2")
    putStrLn ("Block ratio:         " ++ show' br       ++ "")
    putStrLn ("Radius ratio:        " ++ show' dr       ++ "")
    putStrLn ""

syspropPrint = do
    putStrLn "------------------"
    putStrLn "SYSTEM PROPERTIES:"
    putStrLn ("Mach #:              " ++ show' mach     ++ " Mach")
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
    putStrLn "------------------"
    putStrLn "DIAGNOSTICS:"
    putStrLn (if checkMach  then "Mach number check passed."    else "Mach number too high!")
    putStrLn (if checkTPD   then "TPD check passed."            else "TPD check failed!")
    putStrLn (if checkVPD   then "VPD check passed."            else "VPD check failed!")
    putStrLn (if checkTD    then "TD check passed."             else "TD check failed!")
    putStrLn (if checkStack then "Stack check passed."          else "Stack check failed!")
    putStrLn ""

heatExchangerPrint = do
    putStrLn "------------------"
    putStrLn "HEX PROPERTIES:"
    putStrLn ("Diameter:            " ++ show' d1       ++ " mm")
    putStrLn ("Length:              " ++ show' lhex     ++ " mm")
    putStrLn ("Blockage ratio:      " ++ show' br       ++ " mm")
    putStrLn ""


diagnostic = do
    enviroPrint
    gaspropPrint
    dimensionsPrint
    syspropPrint
    heatExchangerPrint
    diagChecks
--    let options = [Title "COP vs x and L"]
--    let options3D = [RangeX 0 1, RangeY 0 1, StepX (2*acc), StepY (2*acc)]
    let options3D = [RangeX 0 1, RangeY 0 0.2, StepX (2*acc), StepY (acc/5)]
    let nonan x l
            | isNaN a       = 0
            | a < 0         = 0
            | a > 100       = 100
            | otherwise     = a
            where
            a = p * sos * xa1 * (qcn x l) --a = qcn x l, a = cop 0.25 l
    let options = [Title "COP vs L"]
    let func = Function3D options options3D nonan
--    plot' [Interactive] X11 func
--    let options2D = [Range 0 1, Step (acc*2)]
--    let func = Function2D options options2D (nonan)
    plot' [Interactive] X11 func
