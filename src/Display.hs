{-# LANGUAGE NoMonomorphismRestriction, GADTs #-}
module Display where

import Input
import Utility
import System
import Speaker
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional
import Diagrams.Prelude hiding (lc, tan)
import Diagrams.Segment
import Diagrams.Backend.Cairo.CmdLine (defaultMain)
import qualified Prelude

cone a m n = polygon with {
                    polyType = PolySides [a1, a1, a2, a2] [cs, m, cs, n]
                } # rotate ((0 !- 90)::Deg) # centerXY
        where
        a1 = Deg (90.0 !+ a)
        a2 = Deg (90.0 !- a)
        cs = psqrt (((n !- m)!/2)!^2 !+ l!^2)
        l = 0.5 !* (m !- n) !* cot (dtr a)

cone' l m n = cone a m n # rotate (Deg (a !- 90.0))
    where
    a = rtd (Prelude.atan ((m !- n) !/ (2.0 !* l)))

speakerDiag a = (rect (spl!/2) 1 ||| cone' (spl!/2) spd 1) # translateX dist
        where
        dist = (lbox !- 1.5!*spl)!/2
        (ulbox, uspl, uspd, ud1) = (cF getBoxLength a, cF getThickness a, cF getInnerD a, getBigTubeD a)
        Dimensional lbox = ulbox / ud1
        Dimensional spl = uspl / ud1
        Dimensional spd = uspd / ud1

cabitubeDiag a = rect lbox d0 <> speakerDiag a              -- Cabinet tube
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

--cabitubeText = dispText lbox " mm" 8 # translateY (d1*0.75)
--inittubeText = dispText lta " mm" 8
--hothexText = dispText lhh " mm" 8 # translateY (d1*0.75)
--stackText = dispText lr " mm" 8
--coldhexText = dispText lhc " mm" 8 # translateY (d1*0.75)
--downconeText = text ""
--thintubeText = dispText lb " mm" 8
--upconeText = text ""
--capText = text ""

cabitube = cabitubeDiag         --    <>  cabitubeText
inittube = inittubeDiag         --    <>  inittubeText
hothex = hothexDiag             --    <>  hothexText
stack = stackDiag               --    <>  stackText
coldhex = coldhexDiag           --    <>  coldhexText
downcone = downconeDiag         --    <>  downconeText
thintube = thintubeDiag         --    <>  thintubeText
upcone = upconeDiag             --    <>  upconeText
cap = capDiag                   --    <>  capText

diag s = cabitube s ||| inittube s ||| hothex s ||| stack s ||| coldhex s ||| downcone s ||| thintube s ||| upcone s ||| cap s

displayDiag a = defaultMain (diag a)
