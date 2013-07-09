module WorkingFluid where

import Utility
import Numeric.Units.Dimensional.Prelude
import qualified Prelude

data GasData = GasData {
                    condition       :: GasCond,
                    model           :: GasModel
                    }

data GasCond = GasCond {
                    pressure        :: Pressure Double,
                    temperature     :: Temperature Double
                    }

data GasModel = GasModel {
                    soundVelocity :: GasCond -> Velocity Double,
                    thermalConductivity :: GasCond -> ThermalConductivity Double,
                    dynamicViscosity :: GasCond -> DynamicViscosity Double,
                    density :: GasCond -> Density Double,
                    cpSpecHeat :: SpecificHeatCapacity Double,
                    cvSpecHeat :: SpecificHeatCapacity Double
                    }

getVal :: (GasModel -> GasCond -> a) -> GasData -> a
getVal f dat = (f (model dat)) (condition dat)

getTemp dat = temperature (condition dat)
getPres dat = pressure (condition dat)
getSV = getVal soundVelocity
getTC = getVal thermalConductivity
getDV = getVal dynamicViscosity
getRHO = getVal density
getCP dat = cpSpecHeat (model dat)
getCV dat = cvSpecHeat (model dat)
getGAM a = (getCP a) / (getCV a)
getPRN a = ((getCP a) * (getDV a)) / (getTC a)

roomCond :: GasCond
roomCond = GasCond (101325.0 *~ pascal) (298.15 *~ kelvin)

-- These models should be within 5% relative error for temperatures
-- between 200 K and 400 K and pressures between 0.5 bar and 50 bar

heliumModel :: GasModel
heliumModel = GasModel sosU kgU muU rhoU cp cv
        where
        sosU (GasCond p t) = (sos (p /~ pascal) (t /~ kelvin)) *~ (meter / second)
        sos p t = 480.59 !+ (1.79 !* t)
        kgU (GasCond p t) = (kg (p /~ pascal) (t /~ kelvin)) *~ (watt / (meter * kelvin))
        kg p t = ((2.3889 `e` (-4)) !* t) !** 0.710
        muU (GasCond p t) = (mu (p /~ pascal) (t /~ kelvin)) *~ (pascal * milli second)
        mu p t = ((7.9639 `e` (-6)) !* t) !** 0.647
        rhoU (GasCond p t) = (rho (p /~ pascal) (t /~ kelvin)) *~ (gram / (milli liter))
        rho p t = (0.4791 !* (p !/ 100000)) !/ (t !+ 0.0000)
        cp = 5.193 *~ (joule / (gram * kelvin))
        cv = 3.116 *~ (joule / (gram * kelvin))

nitrogenModel :: GasModel
nitrogenModel = GasModel sosU kgU muU rhoU cp cv
        where
        sosU (GasCond p t) = (sos (p /~ pascal) (t /~ kelvin)) *~ (meter / second)
        sos p t = 186.77 !+ (0.55 !* t)
        kgU (GasCond p t) = (kg (p /~ pascal) (t /~ kelvin)) *~ (watt / (meter * kelvin))
        kg p t = ((2.9929 `e` (-5)) !* t) !** 0.775
        muU (GasCond p t) = (mu (p /~ pascal) (t /~ kelvin)) *~ (pascal * milli second)
        mu p t = ((1.8417 `e` (-5)) !* t) !** 0.775
        rhoU (GasCond p t) = (rho (p /~ pascal) (t /~ kelvin)) *~ (gram / (milli liter))
        rho p t = (0.3440 !* (p !/ 100000)) !/ (t !+ 2.4543)
        cp = 1.040 *~ (joule / (gram * kelvin))
        cv = 0.743 *~ (joule / (gram * kelvin))

airModel :: GasModel
airModel = GasModel sosU kgU muU rhoU cp cv
        where
        sosU (GasCond p t) = (sos (p /~ pascal) (t /~ kelvin)) *~ (meter / second)
        sos p t = 174.00 !+ (0.58 !* t)
        kgU (GasCond p t) = (kg (p /~ pascal) (t /~ kelvin)) *~ (watt / (meter * kelvin))
        kg p t = ((2.9929 `e` (-5)) !* t) !** 0.775
        muU (GasCond p t) = (mu (p /~ pascal) (t /~ kelvin)) *~ (pascal * milli second)
        mu p t = ((2.1284 `e` (-5)) !* t) !** 0.790
        rhoU (GasCond p t) = (rho (p /~ pascal) (t /~ kelvin)) *~ (gram / (milli liter))
        rho p t = (0.3590 !* (p !/ 100000)) !/ (t !+ 2.4543)
        cp = 1.005 *~ (joule / (gram * kelvin))
        cv = 0.718 *~ (joule / (gram * kelvin))
