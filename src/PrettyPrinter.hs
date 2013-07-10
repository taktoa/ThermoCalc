{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module PrettyPrinter where

import Numeric.Units.Dimensional
import Utility

instance forall d a . (Show d, Show a) => Show (QuantityTr d a) where
    show (QuantityTr db (Dimensional x)) = show x ++ if (null unit) then "" else " " ++ unit
        where unit = prettyPrint db (show (undefined :: d))

data QuantityTr d a = QuantityTr {
                    database :: UnitDB,
                    quantity :: Quantity d a
                    }

type UnitDB = [(String, String)]

prettyPrint' :: Int -> UnitDB -> String -> String
prettyPrint' n db i
        | n == length db                = i
        | i == fst (db !! n)            = snd (db !! n)
        | otherwise                     = prettyPrint' (n !+ 1) db i
        
prettyPrint :: UnitDB -> String -> String
prettyPrint = prettyPrint' 0

stdDB = [("m kg s^-2 A^-1", "T\0183m"),
         ("m^2 kg s^-3 A^-2", "\8486"),
         ("m^2 kg s^-2 A^-2", "H"),
         ("m^-1 kg s^-2", "Pa"),
         ("s^-1", "Hz"),
         ("m s^-1", "m/s"),
         ("m^2", "m\0178"),
         ("m^3", "m\0179"),
         ("m^2 s^-2 K^-1", "m\0178/(K\0183s\0178)"),
         ("kg s^-2", "kg/s\0178"),
         ("kg s^-1", "kg/s"),
         ("m^-1", "1/m"),
         ("m kg s^-3 K^-1", "kg\0183m/(K\0183s\0179)"),
         ("m^-3 kg", "kg/m\0179"),
         ("m^-1 kg s^-1", "Pa\0183s")]
-- \0178 => squared symbol
-- \0179 => cubed symbol
