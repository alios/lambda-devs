{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DEVS.Simulation.Types
    ( SimPorts
    , ProcessorConstructor
    , Processor (..)
    , SimulatorMsg (..)
    , TransportMsg (..)
    ) where


import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Binary
import Control.Distributed.Process
import Data.DEVS.Devs


-- | a touple of 'SendPort' used by 'Processor' for exchange of 'SimulatorMsg' and 'TransportMsg'
type SimPorts m = (SendPort SimulatorMsg, SendPort (TransportMsg m))


-- | type of a processor constructor 
type ProcessorConstructor m = 
    SimPorts m -- ^ 'SimPorts' for processor output
    -> m -- ^ the model run by the processor
    -> Process (SimPorts m) -- ^ returns 'SimPorts' for processor input

-- | a processor runs a model during simulation
class Processor p m where
    data ProcessorState p m :: * -- | the type of the 'Processor's internal state
    mkProcessor :: p -> ProcessorConstructor m -- | a constructor
    proc_s0 :: p -> ProcessorState p m -- | the initial state 

-- | messages used by processors for syncronization
data SimulatorMsg 
    = MsgStar T
    | MsgAt T 
    | MsgDone T
      deriving (Typeable, Data)

-- | messages used by processor for event transportation
data TransportMsg m 
    = MsgY T (Y m)
    | MsgQ T (X m)
      deriving (Typeable)


--- 
--- Instances
---

instance Binary SimulatorMsg where
    put m = do
      put (0x01 :: Word8)
      case m of
        (MsgStar t) -> do put (0x01 :: Word8) ; put t
        (MsgAt t)   -> do put (0x02 :: Word8) ; put t
        (MsgDone t) -> do put (0x03 :: Word8) ; put t
    get = do
      b1 <- getWord8
      b2 <- getWord8
      t <- get
      if (b1 == 0x01) 
      then case b2 of
             0x01 -> return $ MsgStar t 
             0x02 -> return $ MsgAt t
             0x03 -> return $ MsgDone t
             _    -> fail $ "get SimulatorMsg : unknown msg code " ++ show b2
      else fail $ "parseError SimulatorMsg: expected 0x01, got " ++ show b1

instance (DEVS m) => Binary (TransportMsg m) where
    put m = do
      put (0x02 :: Word8)
      case m of
        (MsgY t y) -> do put (0x01 :: Word8) ; put t ; put y
        (MsgQ t x) -> do put (0x02 :: Word8) ; put t ; put x
    get = do
      b1 <- getWord8
      b2 <- getWord8
      t <- get
      if (b1 == 0x02) 
      then case b2 of
             0x01 -> fmap (MsgY t) get
             0x02 -> fmap (MsgQ t) get
             _    -> fail $ "get SimulatorMsg : unknown msg code " ++ show b2
      else fail $ "parseError SimulatorMsg: expected 0x01, got " ++ show b1


