{-# LANGUAGE NoMonomorphismRestriction #-}
module Display where

import Input
import Derived
import Utility (show')
import Diagrams.Prelude hiding (lc)
import Diagrams.Backend.Cairo.CmdLine (defaultMain)

cs = sqrt(((d2 - d1)/2)^2 + lc^2)
ang1 = Deg (90.0 + ang)
ang2 = Deg (90.0 - ang)
cone = polygon with { polyType = PolySides [ang1, ang1, ang2, ang2] [cs, d1, cs, d2] } # rotate (-90::Deg) # centerXY

inittubeDiag = rect lta d1                                  -- Initial tube
hothexDiag = rect (2*lhex) d1                               -- Hot heat exchanger
stackDiag = rect lr d1                                      -- Stack
coldhexDiag = rect lhex d1                                  -- Cold heat exchanger
downconeDiag = cone # rotate (180::Deg)                     -- Downcone
thintubeDiag = rect lb d2                                   -- Thin tube
upconeDiag = cone                                           -- Upcone
capDiag = wedge (d1/2) (270::Deg) (90::Deg)                 -- Cap

inittubeText = text (show' lta ++ " mm") # fontSize 8
hothexText = text ""
stackText = text (show' lr ++ " mm") # fontSize 8
coldhexText = text ""
downconeText = text ""
thintubeText = text (show' lb ++ " mm") # fontSize 8
upconeText = text ""
capText = text ""

inittube = inittubeText <> inittubeDiag
hothex = hothexText <> hothexDiag
stack = stackText <> stackDiag
coldhex = coldhexText <> coldhexDiag
downcone = downconeText <> downconeDiag
thintube = thintubeText <> thintubeDiag
upcone = upconeText <> upconeDiag
cap = capText <> capDiag

combine8 g h i j k l m n = a ||| b
    where
    a = c ||| d
    b = e ||| f
    c = g ||| h
    d = i ||| j
    e = k ||| l
    f = m ||| n

diag = combine8 inittube hothex stack coldhex downcone thintube upcone cap

displayDiag = defaultMain (diag # scale (1/d1))
