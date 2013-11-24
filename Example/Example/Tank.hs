{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Example.Tank where

import Data.DEVS
import Data.Binary
import Data.Typeable (Typeable)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude
import Numeric.NumType (Zero, Pos3, Neg1)


type FlowRateD = Dim Pos3 Zero Neg1 Zero Zero Zero Zero
type FlowRate t = Quantity FlowRateD t

data Tank = Tank {
      tank_initial :: Volume Double
    } deriving (Typeable, Ord, Eq, Show)

instance AtomicModel Tank
instance ProcessorModel Simulator Tank

tank = Tank { tank_initial = 2000 *~ liter }
pm_tank = procModel tank
ref_tank = selfRef tank



data TankCmd = TankAddFlow (FlowRate Double)
             deriving (Typeable, Ord, Eq)



data TankEvent = TankFlowRate (FlowRate Double) | TankEmpty
             deriving (Typeable)

instance Binary TankCmd
instance Binary TankEvent 


instance PDEVS Tank where
    type X Tank = TankCmd
    type Y Tank = TankEvent
    data S Tank = 
        TankState {
          tank_volume :: Volume Double,
          tank_rate :: FlowRate Double
        }
    s0 m = TankState 
           { tank_volume = tank_initial m,
             tank_rate = 0 *~ (liter / second)
           }
    lambda m s = if tankEmpty s
                 then TankEmpty
                 else TankFlowRate $ tank_rate s
    ta m s = if (tankEmpty s || (tank_rate s <= (0 *~ (liter / second))))
             then t_infinity
             else (tank_volume s / tank_rate s)
    delta_int _ s = s 
    delta_ext m s t xs = 
        let cr = tank_rate s
            nv = (tank_volume s) - (cr * t)
            s' = delta_con m s xs
        in  s' { tank_volume = nv }
    delta_con m s xs =
        s { tank_rate = tankFold (tank_rate s) xs }
        where tankFold :: FlowRate Double -> Set TankCmd -> FlowRate Double
              tankFold = Set.fold f
              f :: TankCmd -> FlowRate Double -> FlowRate Double
              f (TankAddFlow r') = (+) r'

tankEmpty :: S Tank -> Bool
tankEmpty s = (tank_volume s <= (0 *~ liter))





