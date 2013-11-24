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

data Coordinator deriving (Typeable)


instance ProcessorType Coordinator


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
                         , coord_procs :: [ProcessorT]
                         }
                                 



    mkProcessor conf' (PM m' mconf) =
      let conf = maybe conf' id mconf
          m = maybe (error $ "mkProcessor of Coordinator must be called with a CoupledModel") id
              (cast  m') 

      in do 
        proc_say conf $ "creating Coordinator for model " ++ show m
        return . MkProcessorT $ CoordinatorState {
                             coord_model = m,
                             coord_procs = []
                           }
               

