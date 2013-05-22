module Diagnostics where

import Input
import Derived
import Graphics.EasyPlot
import Math.Root.Finder
import Data.Function (on)
import Data.List (minimumBy)
import Data.Decimal
import Data.Word

-- Useful functions
places :: Int
places = 12
show' :: Double -> String
show' x
    | al < places       = a ++ replicate (places - al) '0'
    | al > places       = take places a
    | al == places      = a
    where
    a = show (realFracToDecimal ((fromIntegral places)::Word8) x)
    al = length a

-- Checks
small = 0.05
maxMach = 0.1
checkMach = mach < maxMach
--checkStack = Ls < (small/k)
checkTPD = 2*y0*small > dk
checkVPD = 2*y0*small > dv
checkTD = t*small > dt
checkStack = (y0 > (2*dk)) && (y0 < (4*dk))

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
    putStrLn ("Stack spacing:       " ++ show' (y0/2)   ++ " mm")
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

diagnostic = do
    enviroPrint
    gaspropPrint
    dimensionsPrint
    syspropPrint
    diagChecks
    let options = [Title "COP vs x and L"]
    let options3D = [RangeX 0 1, RangeY 0 1, StepX acc, StepY acc]
    --let options3D = [RangeX 0 0.3, RangeY 0 0.3, StepX acc, StepY acc]
    let nonan x l
            | isNaN a       = 0
            | a > copMax    = copMax
            | a < 0         = 0
            | otherwise     = a
            where
            a = (qcn x l) --a = cop x l, a = cop 0.25 l
    let func = Function3D options options3D nonan
    plot' [Interactive] X11 $ func
--    let options = [Title "COP vs L"]
--    let options2D = [Range 0 1, Step (acc*2)]
--    let func = Function2D options options2D nonan
--    plot' [Interactive] X11 $ func


