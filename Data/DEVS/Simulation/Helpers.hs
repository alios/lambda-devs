module Data.DEVS.Simulation.Helpers
    (procError, mkSimPorts) where

import Control.Distributed.Process
import Data.DEVS.Simulation.Types
import Data.DEVS.Devs

-- | outputs 'msg' using 'say' and then calls 'fail'
procError :: String -> Process a
procError msg = say msg >>= fail msg

-- | create new channels for 'SimulatorMsg' and 'TransportMsg'
mkSimPorts :: 
    (DEVS m) => Process (SimPorts m, (ReceivePort SimulatorMsg, ReceivePort (TransportMsg m)))
mkSimPorts = do
      (cs_sim_self, cr_sim_self) <- newChan
      (cs_trans_self, cr_trans_self) <- newChan
      return ((cs_sim_self, cs_trans_self), (cr_sim_self, cr_trans_self))
