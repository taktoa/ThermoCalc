module Main where

import Utility
import Input
import Speaker
import System
import WorkingFluid
import Regenerator
import Diagnostics
--import Display

import Data.ConfigFile
import Control.Monad.Error
import System.Environment

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

ourMeasure :: Double -> Double -> GasModel -> GasData
ourMeasure a b c = GasData cond model
        where
        p = (a `e` 5) *~ pascal
        t = b         *~ kelvin
        cond = GasCond p t
        model = c

ourRawDim :: Double -> Double -> Double -> Double -> Double -> Double -> SpeakerDimData
ourRawDim a b c d e f = SpeakerDimData inner screw total thick md area
        where
        inner = a     *~ milli meter
        screw = b     *~ milli meter
        total = c     *~ milli meter
        thick = d     *~ milli meter
        md = e        *~ milli meter
        area = f      *~ square (milli meter)

ourRawRes :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> SpeakerResData
ourRawRes a b c d e f g = SpeakerResData rf minf maxf cv dmass qm qe
        where
        rf = a        *~ hertz
        minf = b      *~ hertz
        maxf = c      *~ hertz
        cv = d        *~ liter
        dmass = e     *~ gram
        qm = f        *~ one
        qe = g        *~ one

ourRawElec :: Double -> Double -> Double -> Double -> Double -> SpeakerElecData
ourRawElec a b c d e = SpeakerElecData resist induct maxp rmsp imp
        where
        resist = a    *~ ohm
        induct = b    *~ milli henry
        imp = c       *~ ohm
        maxp = d      *~ watt
        rmsp = e      *~ watt

ourGas :: Double -> Double -> GasModel -> GasData
ourGas a b c = GasData cond model
        where
        p = (a `e` 5) *~ pascal
        t = b         *~ kelvin
        cond = GasCond p t
        model = c
ourDim :: Double -> Double -> Double -> Double -> DimData
ourDim a b c d = DimData tl d0 d1 d2
        where
        tl = a        *~ milli meter
        d0 = b        *~ milli meter
        d1 = c        *~ milli meter
        d2 = (c!*d)   *~ milli meter

ourRegen :: Double -> Double -> Double -> RegenData
ourRegen a b c = RegenData hr br dt
        where
        dt = a        *~ kelvin
        hr = b        *~ micro meter
        br = c        *~ one

gasStrParse :: String -> GasModel
gasStrParse "air"       = airModel
gasStrParse "helium"    = heliumModel
gasStrParse "nitrogen"  = nitrogenModel
gasStrParse _           = nitrogenModel

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


main = do
    rv <- runErrorT $ do
        --args <- liftIO $ getArgs
        --let al = length args
        --if al == 0 then error "No config file name given" else liftIO $ putStr ""
        let file = "template.cfg"
        cp <- join $ liftIO $ readfile emptyCP file
        ampdial <- get cp "Amplifier"   "dial"
        wgas    <- get cp "WorkingGas"  "gas"
        wtemp   <- get cp "WorkingGas"  "temp"
        wpres   <- get cp "WorkingGas"  "pres"
        dtl     <- get cp "Dimensions"  "tl"
        dd0     <- get cp "Dimensions"  "d0"
        dd1     <- get cp "Dimensions"  "d1"
        ddr     <- get cp "Dimensions"  "dr"
        rdt     <- get cp "Regenerator" "dt"
        rhr     <- get cp "Regenerator" "hr"
        rbr     <- get cp "Regenerator" "br"
        sdinner <- get cp "SpeakerDim"  "inner"
        sdscrew <- get cp "SpeakerDim"  "screw"
        sdtotal <- get cp "SpeakerDim"  "total"
        sdthick <- get cp "SpeakerDim"  "thick"
        sdmd    <- get cp "SpeakerDim"  "md"
        sdarea  <- get cp "SpeakerDim"  "area"
        srrf    <- get cp "SpeakerRes"  "rf"
        srminf  <- get cp "SpeakerRes"  "minf"
        srmaxf  <- get cp "SpeakerRes"  "maxf"
        srcv    <- get cp "SpeakerRes"  "cv"
        srdmass <- get cp "SpeakerRes"  "dmass"
        srqm    <- get cp "SpeakerRes"  "qm"
        srqe    <- get cp "SpeakerRes"  "qe"
        seresis <- get cp "SpeakerElec" "resist"
        seinduc <- get cp "SpeakerElec" "induct"
        seimped <- get cp "SpeakerElec" "imp"
        semaxp  <- get cp "SpeakerElec" "maxp"
        sermsp  <- get cp "SpeakerElec" "rmsp"
        sggas   <- get cp "SpeakerGas"  "gas"
        sgtemp  <- get cp "SpeakerGas"  "temp"
        sgpres  <- get cp "SpeakerGas"  "pres"
        let wgasM   = gasStrParse wgas
        let sggasM  = gasStrParse sggas
        let cfgGas = ourGas wpres wtemp wgasM
        let cfgDim = ourDim dtl dd0 dd1 ddr
        let cfgRegen = ourRegen rdt rhr rbr
        let cfgInput = InputData cfgGas cfgDim cfgRegen
        let cfgMeasure = ourMeasure sgpres sgtemp sggasM
        let cfgRawRes = ourRawRes srrf srminf srmaxf srcv srdmass srqm srqe
        let cfgRawElec = ourRawElec seresis seinduc seimped semaxp sermsp
        let cfgRawDim = ourRawDim sdinner sdscrew sdtotal sdthick sdmd sdarea
        let cfgSpeaker = SpeakerData cfgMeasure cfgRawRes cfgRawElec cfgRawDim
        let cfgAmplifier = ourAmplifier ampdial
        let cfgSystem = System cfgInput cfgSpeaker cfgAmplifier
        liftIO $ diagnostic cfgSystem
        return "done"
    print rv

