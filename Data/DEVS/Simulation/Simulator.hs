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

{-# LANGUAGE DeriveDataTypeable #-}

module Data.DEVS.Simulation.Simulator
    where

import Data.Typeable (Typeable)
import Control.Distributed.Process
import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude
import qualified Control.Concurrent.MSemN as MSemN
import Control.Concurrent.MSemN (MSemN)
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import qualified Data.Set as Set
import Data.Set (Set)
import Data.DEVS.Simulation.Processor
import Data.DEVS.Devs

data SimState m = SimState {
      sim_tN :: T,
      sim_tL :: T,
      sim_S :: S m,
      sim_bag :: TVar (Set (X m)),
      sim_sem_count :: MSemN Int,
      sim_parent :: SendPort (SimMsg (Set (Y m)))
    } deriving (Typeable)

initSimState :: (AtomicModel m) => SendPort (SimMsg (Set (Y m))) -> Process (SimState m)
initSimState parent = do
  sem_count <- liftIO $ MSemN.new 0
  bag <- liftIO $ newTVarIO Set.empty
  return $ SimState { sim_tN = 0 *~ second
                    , sim_tL = 0 *~ second
                    , sim_S = s0
                    , sim_bag = bag
                    , sim_sem_count = sem_count
                    , sim_parent = parent
                    }


simu :: (AtomicModel m) => 
      ProcessorModel (S m) (X m) (Y m) -> SimState m -> SimMsg (X m) -> 
      Process (SimState m)
simu (SimulatorModel 
     (DeltaInt _delta_int) (DeltaExt _delta_ext) (DeltaCon _delta_con) (Lambda y) (Ta _ta) (_s0))
     sim (Sync x_count t) = do
      sem_count <- liftIO $ MSemN.new x_count

      s' <- if (t == sim_tN sim) 
           then do sendChan (sim_parent sim) $ Out (y (sim_S sim)) t
                   if (x_count == 0) 
                   then return $ _delta_int $ sim_S sim
                   else do
                     waitZero (sim_sem_count sim)
                     sim_bag' <- liftIO . readTVarIO . sim_bag $ sim
                     return $ _delta_con (sim_S sim) sim_bag'
           else do waitZero (sim_sem_count sim)
                   sim_bag' <- liftIO . readTVarIO . sim_bag $ sim
                   return $ _delta_ext (sim_S sim) (t - (sim_tL sim)) sim_bag'
      let tN' = t + _ta (sim_S sim)
      bagN <- liftIO $ newTVarIO Set.empty
      sendChan (sim_parent sim) $ Done tN'
      return sim { sim_tN = tN'
                 , sim_tL = t
                 , sim_S = s'
                 , sim_bag = bagN
                 }
simu (SimulatorModel _ _ _ _ _ _) sim (Out y t) = liftIO $ waitOnZero (sim_sem_count sim) $ do
      atomically $ modifyTVar (sim_bag sim) (Set.insert y)
      MSemN.signal (sim_sem_count sim) 1
      return sim

simu _ _ (Done _) = fail $ "Simulator must not receive a Done Msg"



--simu' :: (AtomicModel m) => m -> SimState m -> SimMsg (X m) -> Process (SimState m)
--simu' m = simu (toSimulatorModel m)



waitZero :: (Integral i) => MSemN i -> Process ()
waitZero sem = liftIO $ MSemN.wait sem 0
waitOnZero :: Integral i => MSemN i -> IO a -> IO a
waitOnZero sem = MSemN.with sem 1

