module Main where

import Input
import Derived
import Diagnostics
import Display (displayDiag)

diagnostic = do
    enviroPrint
    gaspropPrint
    syspropPrint
    cabinetPrint
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

main = diagnostic


