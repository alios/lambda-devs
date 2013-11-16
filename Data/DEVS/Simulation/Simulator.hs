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

module Data.DEVS.Simulation.Simulator
    ( Simulator, mkSimulator ) where

import Data.Binary (Binary)
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Distributed.Process
import Data.DEVS.Devs
import Data.DEVS.Simulation.Types
import Data.DEVS.Simulation.Helpers
import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude


-- | the 'Simulator' type. See also 'Processor'
newtype Simulator m = Simulator m

-- | creates a new 'Simulator' for the given 'DEVS' 
mkSimulator :: (ProcessorModel m, DEVS m) => m -> Simulator m
mkSimulator = Simulator

instance (ProcessorModel m, DEVS m) => Processor (Simulator m) m where
    data ProcessorState (Simulator m) m = 
        SimulatorState {
          as_TL :: T,
          as_TN :: T,
          as_modelS :: S m,
          as_bag :: Set (X m)
        }

    data ProcessorConfig (Simulator m) m = SimulatorConfig 
    defaultProcessorConfig = SimulatorConfig

    proc_s0 (Simulator m) = 
        SimulatorState { as_TL = 0 *~ second
                       , as_TN = 0 *~ second
                       , as_modelS = s0 m
                       , as_bag = Set.empty
                       }

    mkProcessor p cfg' (cs_sim_parent, cs_trans_parent) mod =
        let cfg = readProcessorConfig cfg'
            localLoop cr_self s = updateSimState s cr_self >>= localLoop cr_self
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
          ((cs_sim_self, cs_trans_self), cr_self) <- mkPorts p
          _ <- spawnLocal $ localLoop cr_self initState
          -- TODO store pid in registry ?
          return (cs_sim_self, cs_trans_self)

