{-# LANGUAGE UndecidableInstances, ScopedTypeVariables, EmptyDataDecls, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module PrettyPrinter where

import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional 
import qualified Prelude

instance forall d a . (Show d, Show a) => Show (Quantity d a) where
    show (Dimensional x) = show x ++ if (null unit) then "" else " " ++ unit
        where unit = prettyPrint stdDB (show (undefined :: d))

type UnitDB = [(String, String)]

prettyPrint' :: Int -> UnitDB -> String -> String
prettyPrint' n db i
        | n == length db        = i
        | i == fst (db !! n)    = snd (db !! n)
        | otherwise             = prettyPrint' (n + 1) db i
        
prettyPrint :: UnitDB -> String -> String
prettyPrint = prettyPrint' 0

stdDB = [("m kg s^-2 A^-1", "T m"),
         ("m^2 kg s^-3 A^-2", "ohm"),
         ("m^2 kg s^-2 A^-2", "H"),
         ("m^-1 kg s^-2", "Pa"),
         ("s^-1", "Hz"),
         ("m s^-1", "m/s"),
         ("m^-1 kg s^-1", "Pa s")]
