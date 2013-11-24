{-
Copyright (c) 2013, Markus Barenhoff <alios@alios.org>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the <organization> nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.DEVS.Simulation.Coordinator
    ( Coupled ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable 
import Data.DEVS.Simulation.Types
import Data.DEVS.Devs
import Data.DEVS.Simulation.Types
import Control.Distributed.Process

data Coupled deriving (Typeable)


instance ProcessorType Coupled


instance Processor Coupled where
    data ProcessorConf Coupled = 
        CoordinatorConf { coord_compConfs ::  forall t . (ProcessorType t)
                                             => Map (ProcModelT t) (ProcessorConf t) 
                        }
    defaultProcessorConf = 
        CoordinatorConf { coord_compConfs = Map.empty
                        }


    data ProcessorCore Coupled = 
        CoordinatorState { coord_model :: ProcModelT Coupled 
                         , coord_procs :: [ProcRef]
                         }
                                 



    mkProcessor conf' (PM m' mconf) =
      let conf = maybe conf' id mconf
          m = maybe (error $ "mkProcessor of Coordinator must be called with an CoupledModel") id
              (cast  m') 

      in do 
        proc_say conf $ "creating Coordinator for model " ++ show m
        return . MkProcRef $ CoordinatorState {
                             coord_model = m,
                             coord_procs = []
                           }
               



      

{-

-- | the 'Coordinator' type. See also 'Processor'
newtype Coordinator m = Coordinator m
    deriving (Typeable, Data)

-- | creates a new 'Coordinator' for the given 'CoupledModel'  
mkCoordinator :: (ProcessorModel m, CoupledModel m) => m -> Coordinator m
mkCoordinator = Coordinator

type ChildMap = (ProcessorModel m) => IntMap (ChildRef m)
newtype ChildRef m = ChildRef (m, Set Influencer)
type Influencer = (ProcessorModel m) => (m, Z i m)

instance (ProcessorModel m, CoupledModel m) => Processor (Coordinator m) m where
    data ProcessorState (Coordinator m) m = 
        CoordinatorState {
          cs_TL :: T,
          cs_TN :: T,
          cs_D :: (ProcessorModel a) => Set (ChildRef a)
        }
    data ProcessorConfig (Coordinator m) m = CoordinatorConfig 

    defaultProcessorConfig = CoordinatorConfig

    proc_s0 (Coordinator m) = CoordinatorState {
                  cs_TL = 0 *~ second,
                  cs_TN = 0 *~ second,
                  cs_D = Set.empty
                }
    mkProcessor p cfg (cs_sim_parent, cs_tran_parent) m = 
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
          ((cs_sim_self, cs_trans_self), cr_self) <- mkPorts p
          let initState = proc_s0 p
          _ <- spawnLocal $ localLoop cr_self initState
          return (cs_sim_self, cs_trans_self)

-}
