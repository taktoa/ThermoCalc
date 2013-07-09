module Regenerator where

import Utility
import Numeric.Units.Dimensional.Prelude

data RegenData = RegenData {
                    hydRadius       :: Length Double,
                    blockageR       :: DimlessDouble,
                    tempDiff        :: Temperature Double
                    }

getHydRadius = hydRadius
getBlockRatio = blockageR
getTempDiff = tempDiff
getHydDiam r = _4 #* getHydRadius r
