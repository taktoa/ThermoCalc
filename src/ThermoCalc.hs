module Main where

import Input
import Speaker
import System
import WorkingFluid
import Regenerator
import Diagnostics
import Display
import Numeric.Units.Dimensional.Prelude
import qualified Prelude

ourMeasure = GasData roomCond airModel

ourRawDim = SpeakerDimData inner screw total thick md
        where
        inner = 165.1 *~ milli meter
        screw = 171.5 *~ milli meter
        total = 187.5 *~ milli meter
        thick = 70.0  *~ milli meter
        md = 2.0      *~ milli meter

ourRawRes = SpeakerResData rf minf maxf cv dmass qm qe
        where
        rf = 83.0     *~ hertz
        minf = 90.0   *~ hertz
        maxf = 3000.0 *~ hertz
        cv = 7.93     *~ liter
        dmass = 20    *~ gram       -- arbitrary
        qm = 3.24     *~ one
        qe = 0.89     *~ one

ourRawElec = SpeakerElecData resist induct maxp rmsp imp
        where
        resist = 4.49 *~ ohm
        induct = 1.07 *~ milli henry
        maxp = 100.0  *~ watt
        rmsp = 50.0   *~ watt
        imp = 5.0     *~ ohm        -- not sure

ourSpeaker = SpeakerData ourMeasure ourRawRes ourRawElec ourRawDim

ourGas = GasData cond nitrogenModel
        where
        cond = GasCond p t
        p = 500000    *~ pascal
        t = 298.15    *~ kelvin

ourDim = DimData tl d0 d1 d2
        where
        tl = 750.0    *~ milli meter
        d0 = 202.72   *~ milli meter
        d1 = 77.92    *~ milli meter
        d2 = 40.894   *~ milli meter

ourRegen = RegenData hr br dt
        where
        hr = 0.2925   *~ milli meter
        br = 0.83     *~ one
        dt = 30.0     *~ kelvin

ourInput = InputData ourGas ourDim ourRegen

ourSystem = System ourInput ourSpeaker

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
    displayDiag a

main = diagnostic ourSystem


