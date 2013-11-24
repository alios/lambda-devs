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
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.DEVS.Simulation.Processor 
    (ProcessorType (..), Processor (..), ProcessorT (..), ProcessorModelT (..), ProcessorModel (..)) where

import Data.Typeable (Typeable, cast)
import Control.Distributed.Process
import Control.Distributed.Process.Serializable

import Data.DEVS.Devs


class ProcessorType p 
              


-- | a processor runs a model during simulation
class Processor p where
    data ProcessorConf p :: *
    data ProcessorState p :: *

    -- | returns the processors default configuration
    defaultProcessorConf :: ProcessorConf p

    -- | start a new processor with given config.
    mkProcessor :: ProcessorConf p -> ProcessorModelT p -> Process ProcessorT

    -- | start a new processor with default config.
    mkDefaultProcessor :: ProcessorModelT p -> Process ProcessorT
    mkDefaultProcessor = mkProcessor defaultProcessorConf

    -- | let the processor say a message 
    proc_say :: ProcessorConf p -> String -> Process ()
    proc_say c msg = do say $ msg

    -- | error: will output error message and call 'fail'
    proc_fail :: ProcessorConf p -> String -> Process ()
    proc_fail c err = do proc_say c err ; fail err

-- | a 'Model' which is suitable to be used in a 'Processor'
class (PDEVS m, Serializable (X m), Serializable (Y m), ProcessorType p) => 
    ProcessorModel p m | m -> p where
                       procModelWith ::  Maybe (ProcessorConf p) -> m -> ProcessorModelT p
                       procModelWith conf m = PM m conf
                       procModel :: m -> ProcessorModelT p
                       procModel = procModelWith Nothing


data ProcessorT = forall p . MkProcessorT (ProcessorState p)

data ProcessorModelT p = forall m . (ProcessorModel p m ) => 
                    PM { proc_model :: m
                       , proc_conf :: Maybe (ProcessorConf p)
                       }
                  deriving (Typeable)

instance Show (ProcessorModelT p) where
    show (PM m _) = show m

instance Eq (ProcessorModelT p) where
    (PM a _) == (PM b _) = case (cast b) of
                        Nothing -> False
                        Just b' -> a == b'
                    
instance Ord (ProcessorModelT p) where
    compare (PM a _) (PM b _) = case (cast b) of
                              Nothing -> compare (show a) (show b)
                              Just b' -> compare a b'
