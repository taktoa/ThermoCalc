module Main where
import Data.Function (on)
import Data.List (minimumBy)
import Data.Decimal
import Data.Word
import Graphics.EasyPlot

data Cfg = Cfg { radius :: Double, thickness :: Double, blockage :: Double, hydradius :: Double }

acc = 0.01

log10 x = (log x) / (log 10)

places = (+3) $ round $ abs $ log10 acc

show' :: Double -> String
show' x
    | al < places       = a ++ replicate (places - al) '0'
    | al > places       = take places a
    | al == places      = a
    where
    a = show (realFracToDecimal (fromIntegral places :: Word8) x)
    al = length a

listgen :: (Enum a, Num a) => (a, a) -> a -> [a]
listgen (m,n) d = [m, (m + d) .. n]

bestRoot f brckt d = xs !! index
    where
    xs = listgen brckt d
    index = fst (minimumBy (compare `on` snd) zipped)
    zipped = zip [0 .. length ys] ys
    ys = map f xs

spacing :: Cfg -> Double
spacing cfg = bestRoot func bkt acc
    where
    Cfg r t b h = cfg
    func sp = abs ((calcB cfg sp * calcH cfg sp) - (b * h))
    bkt = (t, r)

width :: Double -> Double -> Int -> Double
width r s i = 2 * sqrt (r^2 - (fi^2 * s^2))
    where
    fi = fromIntegral i

plateList :: Double -> Double -> [Int]
plateList r s = [0 .. (floor (r / s))]

plates :: Double -> Double -> Double
plates r s = sum [width r s i | i <- plateList r s]

calcB :: Cfg -> Double -> Double
calcB (Cfg r t _ _) s = 1 - ((2 * t * (plates r s))/(pi * r^2))

calcH :: Cfg -> Double -> Double
calcH (Cfg r t _ _) s = af / (4*pf)
    where
    p = plates r s
    n = fromIntegral $ length $ plateList r s
    af = (pi * r^2) - (2 * t * p)
    pf = 2 * ((pi * r) - (t * n) + (2 * p))

main = do
    let cfg = Cfg 77.92 0.254 0.916 0.2
    putStr "Radius: "
    putStrLn (show' $ radius cfg)
    putStr "Thickness: "
    putStrLn (show' $ thickness cfg)
    putStr "Spacing: "
    putStrLn (show' $ spacing cfg)
    putStr "Blockage Goal: "
    putStrLn (show' $ blockage cfg)
    putStr "Hydraulic Radius Goal: "
    putStrLn (show' $ hydradius cfg)
    putStr "Blockage Actual: "
    putStrLn (show' $ calcB cfg (spacing cfg))
    putStr "Hydraulic Radius Actual: "
    putStrLn (show' $ calcH cfg (spacing cfg))
    let options2D = [Range (thickness cfg) ((/10) $ radius cfg), Step (acc/10)]
    let fitB x = abs ((calcB cfg x) - (blockage cfg))
    let funcB = Function2D [Title "Blockage Ratio vs Spacing"] options2D fitB
    let fitH x = abs ((calcH cfg x) - (hydradius cfg))
    let funcH = Function2D [Title "Hydraulic Radius vs Spacing"] options2D fitH
    plot X11 [funcB, funcH]
