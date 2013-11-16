{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.DEVS.Simulation.Simulator
    ( Simulator, mkSimulator ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Distributed.Process
import Data.DEVS.Devs
import Data.DEVS.Simulation.Types
import Data.DEVS.Simulation.Helpers


-- | the 'Simulator' type. See also 'Processor'
newtype Simulator m = Simulator m

-- | creates a new 'Simulator' for the given 'DEVS' 
mkSimulator :: (DEVS m) => m -> Simulator m
mkSimulator = Simulator

instance (DEVS m) => Processor (Simulator m) m where
    data ProcessorState (Simulator m) m = 
        SimulatorState {
          as_TL :: T,
          as_TN :: T,
          as_modelS :: S m,
          as_bag :: Set (X m)
        }

    proc_s0 (Simulator m) = 
        SimulatorState { as_TL = 0
                       , as_TN = 0 
                       , as_modelS = s0 m
                       , as_bag = Set.empty
                       }

    mkProcessor p (cs_sim_parent, cs_trans_parent) mod =
        let localLoop cr_self s = updateSimState s cr_self >>= localLoop cr_self
            updateSimState s (cr_sim_self, cr_trans_self) =
                let mMsgAt (MsgAt t) = 
                        if (t == as_TN s)
                        then let y = (lambda mod (as_modelS s)) 
                             in do sendChan cs_trans_parent $ MsgY t y
                                   sendChan cs_sim_parent $ MsgDone t
                                   return s
                        else procError "simulator error: received MsgAt at t != TN"
                    mMsgQ (MsgQ t q) = do
                      sendChan cs_sim_parent $ MsgDone t
                      return $ s { as_bag = Set.insert q (as_bag s) }
                    mMsgStar msg@(MsgStar t) = do
                      s' <- mMsgStar' msg
                      let tn = t + ta mod (as_modelS s')
                      sendChan cs_sim_parent $ MsgDone tn
                      return $ s' { as_TL = t
                                  , as_TN = tn
                                  }              
                    mMsgStar' (MsgStar t)
                        | p_ext t = updSt s True  
                                    (delta_ext mod (as_modelS s) (t - (as_TL s)) (as_bag s))
                        | p_int t = updSt s False 
                                    (delta_int mod (as_modelS s))
                        | p_con t = updSt s True  
                                    (delta_con mod (as_modelS s) (as_bag s))
                        | otherwise = procError "simulator error: received MsgStar at t>TN or t<TL"
                      where p_ext t = (as_TL s <= t) && (t < as_TN s) && (not . Set.null $ as_bag s)
                            p_int t = (as_TN s == t) && (Set.null $ as_bag s)
                            p_con t = (as_TN s == t) && (not . Set.null $ as_bag s)
                            updSt s empt s' = 
                                return s { as_modelS = s'
                                         , as_bag = if empt then Set.empty else as_bag s 
                                         }
                in receiveWait [ matchChan cr_sim_self mMsgAt
                               , matchChan cr_trans_self mMsgQ
                               , matchChan cr_sim_self mMsgStar
                               ]
        in do
          let initState = proc_s0 p
          ((cs_sim_self, cs_trans_self), cr_self) <- mkSimPorts
          _ <- spawnLocal $ localLoop cr_self initState
          -- TODO store pid in registry ?
          return (cs_sim_self, cs_trans_self)

