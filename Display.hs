{-# LANGUAGE NoMonomorphismRestriction #-}
module Display where

import Input
import Derived
import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

displayDiag = defaultMain (circle 1)
