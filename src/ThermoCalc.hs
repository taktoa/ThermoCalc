module Main where

import Utility
import Input
import Speaker
import System
import WorkingFluid
import Regenerator
import Diagnostics
--import Display
import Numeric.Units.Dimensional.Prelude
import qualified Prelude

ourAmplifier :: Double -> Voltage Double
ourAmplifier x
        | x <= 0        = (f 0) *~ volt
        | x >= 1        = (f 1) *~ volt
        | otherwise     = (f x) *~ volt
        where
        f x = c !/ (1 !+ (a !* pexp(-b !* x)))
        a = 644.4
        b = 4.168
        c = 91.64

ourMeasure = GasData roomCond airModel

ourRawDim = SpeakerDimData inner screw total thick md area
        where
        inner = 165.1 *~ milli meter
        screw = 171.5 *~ milli meter
        total = 187.5 *~ milli meter
        thick = 70.0  *~ milli meter
        md = 2.0      *~ milli meter
        area = 12600  *~ square (milli meter)

ourRawRes = SpeakerResData rf minf maxf cv dmass qm qe
        where
        rf = 83.0     *~ hertz
        minf = 90.0   *~ hertz
        maxf = 3000.0 *~ hertz
        cv = 7.93     *~ liter
        dmass = 8.6   *~ gram
        qm = 3.24     *~ one
        qe = 0.89     *~ one

ourRawElec = SpeakerElecData resist induct maxp rmsp imp
        where
        resist = 4.49 *~ ohm
        induct = 1.07 *~ milli henry
        maxp = 100.0  *~ watt
        rmsp = 50.0   *~ watt
        imp = 5.0     *~ ohm

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
        d2 = 42.08    *~ milli meter

ourRegen = RegenData hr br dt
        where
        hr = 0.2925   *~ milli meter
        br = 0.83     *~ one
        dt = 30.0     *~ kelvin

ourInput = InputData ourGas ourDim ourRegen

ourSystem = System ourInput ourSpeaker (ourAmplifier 23)

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
--    displayDiag a

main = diagnostic ourSystem


