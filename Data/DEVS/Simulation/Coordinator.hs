{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.DEVS.Simulation.Coordinator
    ( Coordinator, mkCoordinator ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Distributed.Process
import Data.DEVS.Devs
import Data.DEVS.Simulation.Types
import Data.DEVS.Simulation.Helpers

-- | the 'Coordinator' type. See also 'Processor'
newtype Coordinator m = Coordinator m

-- | creates a new 'Coordinator' for the given 'CoupledModel'  
mkCoordinator :: (CoupledModel m) => m -> Coordinator m
mkCoordinator = Coordinator

instance (CoupledModel m) => Processor (Coordinator m) m where
    data ProcessorState (Coordinator m) m = 
        CoordinatorState {
          cs_TL :: T,
          cs_TN :: T
        }
    proc_s0 (Coordinator m) = CoordinatorState {
                  cs_TL = 0,
                  cs_TN = 0
                }
    mkProcessor p (cs_sim_parent, cs_tran_parent) m = 
        let localLoop cr_self s = updateCoordState s cr_self >>= localLoop cr_self
            updateCoordState s (cr_sim_self, cr_trans_self) = 
                let mMsgAt (MsgAt t) = 
                        if (t == cs_TN s)
                        then do
                          sendChan cs_sim_parent $ MsgDone t
                          return $ s { cs_TL = t }
                        else procError "coordinator error: received MsgAt at t != TN"
                in receiveWait [ matchChan cr_sim_self mMsgAt]
        in do
          ((cs_sim_self, cs_trans_self), cr_self) <- mkSimPorts
          let initState = proc_s0 p
          _ <- spawnLocal $ localLoop cr_self initState
          return (cs_sim_self, cs_trans_self)
