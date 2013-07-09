module Regenerator where

import Utility
import Numeric.Units.Dimensional.Prelude (Length)

data RegenData = RegenData {
                    hydRadius       :: Length Double,
                    blockageR       :: Double,
                    tempDiff        :: Temperature Double
                    }

getHydRadius = hydRadius
getBlockRatio = blockageR
getTempDiff = tempDiff
getHydDiam r = _4 * getHydRadius r
