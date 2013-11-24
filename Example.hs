module Main where

import Data.DEVS.Simulation
import Example.Ship



{- 

--
-- SHIP 
--
data Ship = Ship {
      ship_mass :: Mass Double,
      ship_init_x :: Length Double,
      ship_init_y :: Length Double,
      ship_engine :: Engine
} deriving (Typeable)

data ShipCmd = ShipSetSpeed (Velocity Double) | ShipSetHeading (PlaneAngle Double)
             deriving (Eq, Ord)
data ShipEvent = ShipEventEngines Bool

instance Model Ship where
    type X Ship = ShipCmd
    type Y Ship = ShipEvent
    data S Ship = 
        ShipState {
          ship_x :: Length Double,
          ship_y :: Length Double,
          ship_bearing :: PlaneAngle Double,
          ship_vx :: Velocity Double,
          ship_vy :: Velocity Double,
          ship_thrust :: Thrust Double,
          ship_set_hdg :: PlaneAngle Double,
          ship_set_v :: Velocity Double
        }
    s0 ship = ShipState 
              { ship_x = ship_init_x ship
              , ship_y = ship_init_y ship
              , ship_bearing = 0 *~ degree
              , ship_vx = 0 *~ (meter / second)
              , ship_vy = 0 *~ (meter / second)
              , ship_thrust = 0 *~ (newton)
              , ship_set_hdg = 0 *~ degree
              , ship_set_v = 0 *~ (meter / second)
              }

instance DEVS Ship where
    lambda m s = undefined --ShipEventEngines (ship_v s == ship_set_v s)
    ta m s = t_infinity
    delta_int m s = s
    delta_ext m s t xs =
        let a = ((ship_thrust s) / (ship_mass m))
            ax = a * cos (ship_bearing s) 
            ay = a * sin (ship_bearing s)
            s' = s { ship_vx = ship_vx s + (ax * t)
                   , ship_vy = ship_vy s + (ay * t)
                   , ship_x = ship_x s + (ship_vx s * t)
                   , ship_y = ship_y s + (ship_vy s * t)
                   } 
        in s
    delta_con m s xs = undefined



--x = f / m

eng1 = Engine { engine_a = 1000 *~ newton, engine_rate = 13 *~ (liter / minute)}
eng1_sim = engineSim eng1
                     
instance CoupledModel Ship

--
-- TANK
--



instance DEVS Tank where
                         






instance ProcessorModel Tank where
--
-- ENGINE 
--

data Engine = Engine {
      engine_a :: Thrust Double,
      engine_rate :: FlowRate Double
    } deriving (Typeable)

data EngineCmd = EngineEnable | EngineDisable
               deriving (Ord, Eq)

instance Binary EngineCmd where

data EngineEvent = EngineEventThrust (Thrust Double)
              deriving (Typeable)

instance Binary EngineEvent where

instance Model Engine where
    type X Engine = EngineCmd
    type Y Engine = EngineEvent
    data S Engine =
        EngineState {
          engine_enabled :: Bool
        }
    s0 _ = EngineState { engine_enabled = False }

instance DEVS Engine where
    lambda m s = EngineEventThrust $ 
                 if (engine_enabled s) 
                 then engine_a m
                 else 0 *~ newton
    delta_int _ s = s
    delta_ext m s t xs = delta_con m s xs
    delta_con m s xs =
        let es = engine_enabled s
            sw = Set.member (if es then EngineDisable else EngineEnable) xs
            es' = if sw then not es else es
        in s { engine_enabled = es' }
    ta m _ = t_infinity

instance ProcessorModel Engine where


engineSim :: Engine -> Simulator Engine
engineSim = mkSimulator

-}
