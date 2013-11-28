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

module Data.DEVS.Simulation.Coordinator ( Coordinator, Processor (..)) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable 
import Data.DEVS.Devs
import Data.DEVS.Simulation.Processor
import Control.Distributed.Process
import Control.Monad.State

data Coordinator deriving (Typeable)


instance ProcessorType Coordinator


data ReceivedMessage

type MessageBag = Set ReceivedMessage

coord_imm :: T -> ProcessorState Coordinator -> forall t . Set (ProcessorT t)
coord_imm t = Map.keysSet . Map.filter ((==) t) . coord_procs

coord_inn :: T -> ProcessorState Coordinator -> forall t . Set (ProcessorT t)
coord_inn t s =
    let imm = coord_imm t s
        inn_filter p = case Map.lookup p $ coord_bags s of
                         Nothing -> False
                         Just b -> not (Set.null b)
    in Set.filter inn_filter imm

instance Processor Coordinator where
    data ProcessorConf Coordinator = 
        CoordinatorConf { coord_compConfs ::  forall t . (ProcessorType t)
                                             => Map (ProcessorModelT t) (ProcessorConf t) 
                        }
    defaultProcessorConf = 
        CoordinatorConf { coord_compConfs = Map.empty
                        }


    data ProcessorState Coordinator = 
        CoordinatorState { coord_model :: ProcessorModelT Coordinator 
                         , coord_procs :: forall t . Map (ProcessorT t) T
                         , coord_bags  :: forall t . Map (ProcessorT t) MessageBag
                         }
                                 
    proc_s0 m = CoordinatorState { coord_model = m, coord_procs = Map.empty, coord_bags = Map.empty }
    proc_onSync c p x_count t = 
        let imm = coord_imm t (proc_state p)
            inn = coord_inn t (proc_state p)
            es = Set.toList $ Set.union imm inn -- (Set (ProcessorT t0)
            isSelfE e = e == p
            f e = do return ()
        in do _ <- sequence $ map f es
              return ()

--    proc_onSync conf p i t = do
      
{-
    mkProcessor conf' proc_out_schan (PM m' mconf) =
      let conf = maybe conf' id mconf
          m = maybe (error $ "mkProcessor of Coordinator must be called with a CoupledModel") id
              (cast  m') 

      in do 
        proc_say conf $ "creating Coordinator for model " ++ show m
        (proc_in_schan, proc_in_rchan) <- newChan
        put $ CoordinatorState 
                { coord_model = m
                , coord_procs = []
                }

        return proc_in_schan 
-}

