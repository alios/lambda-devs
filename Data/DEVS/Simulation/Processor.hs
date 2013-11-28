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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.DEVS.Simulation.Processor 
    (ProcessorType (..), Processor (..), mkProcessor, ProcessorT (..), ProcessorModelT (..), ProcessorModel (..), ProcessorMsg (..)) where

import Data.Typeable (Typeable, cast)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable
import Data.Binary
import Data.DEVS.Devs
import Control.Monad.State (MonadState)
import qualified Control.Monad.State as ST
import Control.Concurrent.STM.TSem
import Control.Monad.STM

data ProcessorMsg = SyncMsg (Int, T)
                  | DoneMsg T
--                  | ExtOutMsg (T, Y m)
    deriving (Typeable)

class ProcessorType p 
             
-- | a processor runs a model during simulation
class (ProcessorType p) => Processor p where
    data ProcessorConf p :: *
    data ProcessorState p :: *

    -- | returns the processors default configuration
    defaultProcessorConf :: ProcessorConf p

    proc_init :: ProcessorConf p -> ProcessorT p -> Process ()
    proc_onSync :: ProcessorConf p -> ProcessorT p -> Int -> T -> Process ()
    proc_onDone :: ProcessorConf p -> ProcessorT p -> T -> Process ()
 
    -- | returns initial ProcessorState 
    proc_s0 :: ProcessorModelT p -> ProcessorState p
   
    -- | let the processor say a message 
    proc_say :: ProcessorConf p -> String -> Process ()
    proc_say c msg = do say $ msg

    -- | error: will output error message and call 'fail'
    proc_fail :: ProcessorConf p -> String -> Process ()
    proc_fail c err = do proc_say c err ; fail err


-- | a 'Model' which is suitable to be used in a 'Processor'
class (PDEVS m, Serializable (X m), Serializable (Y m), ProcessorType p) => 
    ProcessorModel p m | m -> p where
                       procModel :: m -> ProcessorModelT p
                       procModel = PM


data ProcessorT p = MkProcessorT 
    { proc_state :: ProcessorState p
    , proc_rport :: ReceivePort ProcessorMsg
    , proc_sport :: SendPort ProcessorMsg
    , proc_count :: TSem
    , proc_tn :: T
    } 

proc_sendPortId :: ProcessorT t -> SendPortId
proc_sendPortId = sendPortId . proc_sport

instance Ord (ProcessorT t) where
    compare p1 p2 = 
        let spid p = sendPortId $ proc_sport p
        in compare (proc_sendPortId p1) (proc_sendPortId p2)

instance Eq (ProcessorT t)where
    p1 == p2 = (proc_sendPortId p1) == (proc_sendPortId p2)

mkProcessor :: (Processor p, ProcessorModel p m) =>
  ProcessorModelT p -> SendPort (ProcessorMsg) -> T -> Process (ProcessorT p)
mkProcessor m proc_out_sport t = 
    let s0 = proc_s0 m
    in do 
      (proc_in_sport, proc_in_rport) <- newChan
      sem_count <- liftIO . atomically $ newTSem 0
      return $ MkProcessorT
                 { proc_state = s0
                 , proc_rport = proc_in_rport
                 , proc_sport = proc_out_sport
                 , proc_tn = t
                 , proc_count = sem_count
                 }


runProcessorWith :: (Processor p) => ProcessorConf p -> ProcessorT p -> Process ()
runProcessorWith conf p = do
  proc_init conf p
  proc_main
      where proc_main = do
              
              proc_main
            mrport = matchChan (proc_rport p) 
            m_sync = mrport $ \(SyncMsg (i, t)) -> spawnLocal $ proc_onSync conf p i t
            m_done = mrport $ \(DoneMsg t) -> spawnLocal $ proc_onDone conf p t
--            m_extmsg = mrport $ \(ExtOutMsg (t, y)) -> spawnLocal $ proc_onExtOut conf p t y



data ProcessorModelT p = forall m . (ProcessorModel p m ) => 
                    PM { proc_model :: m }
                  deriving (Typeable)

instance Show (ProcessorModelT p) where
    show (PM m) = show m

instance Eq (ProcessorModelT p) where
    (PM a) == (PM b) = case (cast b) of
                        Nothing -> False
                        Just b' -> a == b'
                    
instance Ord (ProcessorModelT p) where
    compare (PM a) (PM b) = case (cast b) of
                              Nothing -> compare (show a) (show b)
                              Just b' -> compare a b'

instance Binary (ProcessorMsg) where
    put m = do
      put (0x23 :: Word8)
      case m of
        (SyncMsg (t, i)) -> put (0x00 :: Word8) >> put t >> put i
        (DoneMsg t) -> put (0x01 :: Word8) >> put t
--      (ExtOutMsg (t, y)) -> put (0x02 :: Word8) >> put t >> put y
    get = do
      magic <- (getWord8)
      if (magic /= 0x23)
         then fail $ "expected ProcessorMsg byte 0x23 got " ++ show magic
         else do b <- getWord8
                 case b of
                   0x00 -> do t <- get ; i <- get ; return $ SyncMsg (t, i)
                   0x01 -> do t <- get ; return $ DoneMsg t
--                 0x02 -> do t <- get ; y <- get ; return $ ExtOutMsg (t, y)
                   t -> fail $ "expected ProcessorMsg type (0-2) got " ++ show t
