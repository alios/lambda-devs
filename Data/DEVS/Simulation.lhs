\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.DEVS.Simulation
    ( Simulator, Coordinator
    , mkProcessor
    ) where

import Data.Binary
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Distributed.Process

import Data.DEVS.Devs
import Data.DEVS.Simulation.Types

\end{code}



\begin{code}

data Simulator = Sim
data Coordinator = Coord

instance (CoupledModel m) => Processor Coordinator m where
    data ProcessorState Coordinator m = 
        CoordinatorState {
          cs_TL :: T,
          cs_TN :: T
        }

    mkProcessor _ (cs_sim_parent, cs_tran_parent) m = 
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
          let initState = CoordinatorState {
                            cs_TL = 0,
                            cs_TN = 0
                          }
          _ <- spawnLocal $ localLoop cr_self initState
          return (cs_sim_self, cs_trans_self)

 
instance (DEVS m) => Processor Simulator m where
    data ProcessorState Simulator m = 
        SimulatorState {
          as_TL :: T,
          as_TN :: T,
          as_modelS :: S m,
          as_bag :: Set (X m)
        }


    mkProcessor _ (cs_sim_parent, cs_trans_parent) mod =
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
          let initState = 
                  SimulatorState { as_TL = 0
                                 , as_TN = 0
                                 , as_modelS = s0 mod
                                 , as_bag = Set.empty
                                 }
          ((cs_sim_self, cs_trans_self), cr_self) <- mkSimPorts
          _ <- spawnLocal $ localLoop cr_self initState
          -- TODO store pid in registry ?
          return (cs_sim_self, cs_trans_self)

--
-- instances 
--

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
        

--
-- utilities
--

procError :: String -> Process a
procError msg = say msg >>= fail msg

mkSimPorts :: 
    (DEVS m) => Process (SimPorts m, (ReceivePort SimulatorMsg, ReceivePort (TransportMsg m)))
mkSimPorts = do
      (cs_sim_self, cr_sim_self) <- newChan
      (cs_trans_self, cr_trans_self) <- newChan
      return ((cs_sim_self, cs_trans_self), (cr_sim_self, cr_trans_self))


\end{code}



class HasInfinite s where
    infinity :: s 

instance (RealFloat f) => HasInfinite f where
    infinity = encodeFloat (floatRadix 0 - 1) (snd $ floatRange 0)
