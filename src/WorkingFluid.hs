module WorkingFluid where

import Numeric.Units.Dimensional.Prelude
import qualified Prelude

type Temperature = ThermodynamicTemperature

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

getVal :: (GasCond -> a) -> GasData -> a
getVal f dat = (f (model dat)) (condition dat)

getSV = getVal soundVelocity
getTC = getVal thermalConductivity
getDV = getVal dynamicViscosity
getRHO = getVal density
getCP = getVal cpSpecHeat
getCV = getVal cvSpecHeat
getGAM a = (getCP a) / (getCV a)
getPRN a = (getCP a) * (getDV a) / (getTC a)

heliumModel :: GasModel
heliumModel = GasModel sosU kgU muU rhoU cp cv
        where
        sosU = \GasCond p t -> (sos (p /~ bar) (t /~ kelvin)) ~* (meter / second)
        sos p t = 480.59 + (1.79 * t)
        kgU = \GasCond p t -> (kg (p /~ bar) (t /~ kelvin)) ~* (watt / (meter * kelvin))
        kg p t = ((2.3889 `e` (-4)) * t) ** 0.710
        muU = \GasCond p t -> (mu (p /~ bar) (t /~ kelvin)) ~* (pascal * milli second)
        mu p t = ((7.9639 `e` (-6)) * t) ** 0.647
        rhoU = \GasCond p t -> (rho (p /~ bar) (t /~ kelvin)) ~* (gram / (milli liter))
        rho p t = (0.4791 * p) / (t + 0.0000)
        cp = 5.193 *~ (joule / (gram * kelvin))
        cv = 3.116 *~ (joule / (gram * kelvin))

nitrogenModel :: GasModel
nitrogenModel = GasModel sosU kgU muU rhoU cp cv
        where
        sosU = \GasCond p t -> (sos (p /~ bar) (t /~ kelvin)) ~* (meter / second)
        sos p t = 186.77 + (0.55 * t)
        kgU = \GasCond p t -> (kg (p /~ bar) (t /~ kelvin)) ~* (watt / (meter * kelvin))
        kg p t = ((2.9929 `e` (-5)) * t) ** 0.775
        muU = \GasCond p t -> (mu (p /~ bar) (t /~ kelvin)) ~* (pascal * milli second)
        mu p t = ((1.8417 `e` (-5)) * t) ** 0.775
        rhoU = \GasCond p t -> (rho (p /~ bar) (t /~ kelvin)) ~* (gram / (milli liter))
        rho p t = (0.3440 * p) / (t + 2.4543)
        cp = 1.040 *~ (joule / (gram * kelvin))
        cv = 0.743 *~ (joule / (gram * kelvin))

airModel :: GasModel
airModel = GasModel sosU kgU muU rhoU cp cv
        where
        sosU = \GasCond p t -> (sos (p /~ bar) (t /~ kelvin)) ~* (meter / second)
        sos p t = 174.00 + (0.58 * t)
        kgU = \GasCond p t -> (kg (p /~ bar) (t /~ kelvin)) ~* (watt / (meter * kelvin))
        kg p t = ((2.9929 `e` (-5)) * t) ** 0.775
        muU = \GasCond p t -> (mu (p /~ bar) (t /~ kelvin)) ~* (pascal * milli second)
        mu p t = ((2.1284 `e` (-5)) * t) ** 0.790
        rhoU p t = \GasCond p t -> (rho (p /~ bar) (t /~ kelvin)) ~* (gram / (milli liter))
        rho p t = (0.3590 * p) / (t + 2.4543)
        cp = 1.005 *~ (joule / (gram * kelvin))
        cv = 0.718 *~ (joule / (gram * kelvin))
