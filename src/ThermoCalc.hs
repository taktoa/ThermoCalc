module Main where

import Input
import Derived
import Diagnostics
import Display (displayDiag)

diagnostic :: System -> IO ()
diagnostic a = do
    enviroPrint a
    gaspropPrint a
    syspropPrint a
    cabinetPrint a
    speakerPrint a
    bigTubePrint a
    heatExchangerPrint a
    regenPrint a
    smallTubePrint a
    conePrint a
    capPrint a
    diagChecks a
    displayDiag a

main = diagnostic


