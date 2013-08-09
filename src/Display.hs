{-# LANGUAGE NoMonomorphismRestriction, GADTs #-}
module Display where

import Input
import Utility
import System
import Speaker
import Diagnostics
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional
import Diagrams.Prelude hiding (lc, tan)
import Diagrams.Segment
import Diagrams.Backend.Cairo.CmdLine (defaultMain)
import qualified Prelude

cone a m n = polygon with {
                    polyType = PolySides [a1, a1, a2, a2] [cs, m, cs, n]
                } # centerXY # rotate ((0.0 !- 90.0)::Deg) # centerXY
        where
        a1 = Deg (90.0 !+ a)
        a2 = Deg (90.0 !- a)
        cs = ppyth ((n !- m)!/2.0) l
        l = 0.5 !* (m !- n) !* cot (dtr a)

cone' l m n = cone a m n # rotate (Deg (a !- 90.0))
    where
    a = rtd (Prelude.atan ((m !- n) !/ (2.0 !* l)))

speakerDiag a = (rect (spl!/2) 1 ||| cone' (spl!/2) spd 1) # translateX dA # translateX dB
        where
        dA = lbox !/ 2
        dB = (0!-0.75)!*spl
        (ulbox, uspl, uspd, ud1) = (cF getBoxLength a, cF getThickness a, cF getInnerD a, getBigTubeD a)
        Dimensional lbox = ulbox / ud1
        Dimensional spl = uspl / ud1
        Dimensional spd = uspd / ud1

cabitubeDiag a = rect lbox d0 <> speakerDiag a   -- Cabinet tube
        where
        (ulbox, ud0, ud1) = ((cF getBoxLength) a, (cF getBoxD) a, getBigTubeD a)
        Dimensional lbox = ulbox / ud1
        Dimensional d0 = ud0 / ud1
inittubeDiag a = rect lta 1                                 -- Initial tube
        where
        (ulta, ud1) = (getTotalLength a, getBigTubeD a)
        Dimensional lta = ulta / ud1
hothexDiag a = rect (2!*lhex) 1                             -- Hot heat exchanger
        where
        (ulhex, ud1) = (getHEXLength a, getBigTubeD a)
        Dimensional lhex = ulhex / ud1
stackDiag a = rect lr 1                                     -- Stack
        where
        (ulr, ud1) = (getRegenLength a, getBigTubeD a)
        Dimensional lr = ulr / ud1
coldhexDiag a = rect lhex 1                                 -- Cold heat exchanger
        where
        (ulhex, ud1) = (getHEXLength a, getBigTubeD a)
        Dimensional lhex = ulhex / ud1
downconeDiag a = cone cang 1 d2 # rotate (180::Deg)         -- Downcone
        where
        (ud1, ud2) = (getBigTubeD a, getSmallTubeD a)
        Dimensional d2 = ud2 / ud1
thintubeDiag a = rect lb d2                                 -- Thin tube
        where
        (ulb, ud1, ud2) = (getSmallTubeLength a, getBigTubeD a, getSmallTubeD a)
        Dimensional lb = ulb / ud1
        Dimensional d2 = ud2 / ud1
upconeDiag a = cone cang 1 d2                               -- Upcone
        where
        (ud1, ud2) = (getBigTubeD a, getSmallTubeD a)
        Dimensional d2 = ud2 / ud1
capDiag a = wedge (0.5) (270::Deg) (90::Deg)                -- Cap

--pl = 1

--dispText num unit size = text (show_ pl (round_ pl num) ++ unit) # fontSize size

--labelline = straight a b
    --where
    --a = location (thintubeDiag # alignL)
    --b = location thintubeText
    --c = location (thintubeDiag # alignR)
    
fs = 0.1
sz = 1
pl = 1

disp n p u = text ((show (round_ p n)) ++ u) # fontSize fs

cabitubeText a = disp lbox pl " mm" # translateY 1
        where
        Dimensional lbox = ((cF getBoxLength) a) / (1 *~ milli meter)
inittubeText a = disp lta pl " mm"
        where
        Dimensional lta = (getTotalLength a) / (1 *~ milli meter)
hothexText a = disp lhh pl " mm" # translateY 1
        where
        Dimensional lhh = (getHEXLength a) / (0.5 *~ milli meter)
stackText a = disp lr pl " mm"
        where
        Dimensional lr = (getRegenLength a) / (1 *~ milli meter)
coldhexText a = disp lhc pl " mm" # translateY 1
        where
        Dimensional lhc = (getHEXLength a) / (1 *~ milli meter)
downconeText a = text ""
thintubeText a = disp lb pl " mm"
        where
        Dimensional lb = (getSmallTubeLength a) / (1 *~ milli meter)
upconeText a = text ""
capText a = text ""

cabitube a = cabitubeDiag a             <>  cabitubeText a
inittube a = inittubeDiag a             <>  inittubeText a
hothex a = hothexDiag a                 <>  hothexText a
stack a = stackDiag a                   <>  stackText a
coldhex a = coldhexDiag a               <>  coldhexText a
downcone a = downconeDiag a             <>  downconeText a
thintube a = thintubeDiag a             <>  thintubeText a
upcone a = upconeDiag a                 <>  upconeText a
cap a = capDiag a                       <>  capText a

diag s = (cabitube s ||| inittube s ||| hothex s ||| stack s ||| coldhex s ||| downcone s ||| thintube s ||| upcone s ||| cap s) # full
        where
        full = if checkAll s then scale 1 else bg red

displayDiag a = defaultMain (diag a)
