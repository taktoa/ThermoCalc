{-# LANGUAGE NoMonomorphismRestriction #-}
module Display where

import Input
import Derived
import Utility (show_)
import Diagrams.Prelude hiding (lc, tan)
import Diagrams.Backend.Cairo.CmdLine (defaultMain)

cone a m n = polygon with {
                polyType = PolySides [a1, a1, a2, a2] [cs, m, cs, n] } # rotate (-90::Deg) # centerXY
        where
        a1 = Deg (90.0 + a)
        a2 = Deg (90.0 - a)
        cs = sqrt(((n - m)/2)^2 + l^2)
        l = (m - n) / (2.0 * tan (pi*a/180))
        
        

cone' l m n = (cone a m n) # rotate (Deg (a - 90.0))
    where
    a = 180 * atan ((m - n) / (2.0 * l))/pi

speakerDiag = (rect (splen/2) d1 ||| cone' (splen/2) spdsmall d1) # translateX dist
    where
    dist = (lbox - 1.5*splen)/2
cabitubeDiag = rect lbox d0 <> speakerDiag                  -- Cabinet tube
inittubeDiag = rect lta d1                                  -- Initial tube
hothexDiag = rect (2*lhex) d1                               -- Hot heat exchanger
stackDiag = rect lr d1                                      -- Stack
coldhexDiag = rect lhex d1                                  -- Cold heat exchanger
downconeDiag = cone ang d1 d2 # rotate (180::Deg)           -- Downcone
thintubeDiag = rect lb d2                                   -- Thin tube
upconeDiag = cone ang d1 d2                                 -- Upcone
capDiag = wedge (d1/2) (270::Deg) (90::Deg)                 -- Cap

dig = 5

cabitubeText = text ""
inittubeText = text (show_ dig lta ++ " mm") # fontSize 8
hothexText = text "Testing" # translateY 30
stackText = text (show_ dig lr ++ " mm") # fontSize 8
coldhexText = text ""
downconeText = text ""
thintubeText = text (show_ dig lb ++ " mm") # fontSize 8
upconeText = text ""
capText = text ""

cabitube = cabitubeText <> cabitubeDiag
inittube = inittubeText <> inittubeDiag
hothex = hothexText <> hothexDiag
stack = stackText <> stackDiag
coldhex = coldhexText <> coldhexDiag
downcone = downconeText <> downconeDiag
thintube = thintubeText <> thintubeDiag
upcone = upconeText <> upconeDiag
cap = capText <> capDiag

diag = cabitube ||| inittube ||| hothex ||| stack ||| coldhex ||| downcone ||| thintube ||| upcone ||| cap

displayDiag = defaultMain (diag # scale (1/d1))
