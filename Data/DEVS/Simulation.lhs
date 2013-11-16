\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DEVS.Simulation
    ( Simulator, mkSimulator
    , Coordinator, mkCoordinator
    ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Distributed.Process

import Data.DEVS.Devs
import Data.DEVS.Simulation.Types

\end{code}


\section{Simulator}

\begin{code}

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
\end{code}

\section {Coordinator}

\begin{code}

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
\end{code}



\section {Utility functions}

\begin{code}

procError :: String -> Process a
procError msg = say msg >>= fail msg

mkSimPorts :: 
    (DEVS m) => Process (SimPorts m, (ReceivePort SimulatorMsg, ReceivePort (TransportMsg m)))
mkSimPorts = do
      (cs_sim_self, cr_sim_self) <- newChan
      (cs_trans_self, cr_trans_self) <- newChan
      return ((cs_sim_self, cs_trans_self), (cr_sim_self, cr_trans_self))


\end{code}


