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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.DEVS.Simulation.Coordinator
    ( Coordinator, mkCoordinator ) where

import Data.Binary (Binary)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Distributed.Process
import Data.DEVS.Devs
import Data.DEVS.Simulation.Types
import Data.DEVS.Simulation.Helpers

import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude

-- | the 'Coordinator' type. See also 'Processor'
newtype Coordinator m = Coordinator m

-- | creates a new 'Coordinator' for the given 'CoupledModel'  
mkCoordinator :: (CoupledModel m) => m -> Coordinator m
mkCoordinator = Coordinator

instance (CoupledModel m, Binary T) => Processor (Coordinator m) m where
    data ProcessorState (Coordinator m) m = 
        CoordinatorState {
          cs_TL :: T,
          cs_TN :: T
        }
    proc_s0 (Coordinator m) = CoordinatorState {
                  cs_TL = 0 *~ second,
                  cs_TN = 0 *~ second
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
