{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DEVS.Simulation.Types
    ( SimulatorMsg (..)
    , TransportMsg (..)
    , SimPorts
    , ProcessorConstructor
    , Processor (..)
    ) where



import Data.Typeable
import Data.Data

import Control.Distributed.Process

import Data.DEVS.Devs


data SimulatorMsg 
    = MsgStar T
    | MsgAt T
    | MsgDone T
      deriving (Typeable, Data)

data TransportMsg m 
    = MsgY T (Y m)
    | MsgQ T (X m)
      deriving (Typeable)

type SimPorts m = (SendPort SimulatorMsg, SendPort (TransportMsg m))


-- | type of a processor constructor 
type ProcessorConstructor m = 
    SimPorts m -- ^ 'SimPorts' for processor output
    -> m -- ^ the model run by the processor
    -> Process (SimPorts m) -- ^ returns 'SimPorts' for processor input

class Processor p m where
    data ProcessorState p m :: *
    mkProcessor :: p -> ProcessorConstructor m

