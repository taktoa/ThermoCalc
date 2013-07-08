module WorkingFluid where

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
        sos p t = 480.589 + (1.7875 * t)
        kgU = \GasCond p t -> (kg (p /~ bar) (t /~ kelvin)) ~* (watt / (meter * kelvin))
        kg p t = ((2.38889 `e` (-4)) * t) ** 0.710
        muU = \GasCond p t -> (mu (p /~ bar) (t /~ kelvin)) ~* (pascal * milli second)
        mu p t = ((7.96389 `e` (-6)) * t) ** 0.647
        rhoU = \GasCond p t -> (rho (p /~ bar) (t /~ kelvin)) ~* (gram / (milli liter))
        rho p t = (0.4791 * p) / t
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
        rho p t = (1.242 * p) / ((3.61 * t) + 8.86)
        cp = 1.040 *~ (joule / (gram * kelvin))
        cv = 0.743 *~ (joule / (gram * kelvin))
