module Main where

import Input
import Derived
import Diagnostics
import Display (displayDiag)

diagnostic :: InputData -> IO ()
diagnostic i = do
    enviroPrint i
    gaspropPrint i
    syspropPrint i
    cabinetPrint i
    speakerPrint i
    bigTubePrint i
    heatExchangerPrint i
    regenPrint i
    smallTubePrint i
    conePrint i
    capPrint i
    diagChecks i
    displayDiag i

main = diagnostic


