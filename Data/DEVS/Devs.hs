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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}


-- | The /Discrete Event System Sepcification (DEVS)/ formalism defines
--   discrete event simulation models in a hierachical, modular manner.
module Data.DEVS.Devs 
    ( -- * Parallel DEVS
      Model (..), DeltaInt (..), DeltaExt (..), DeltaCon (..), Lambda (..), Ta (..)
      -- * Time base 
    , T, t_infinity
      -- * AtomicModel  
    , AtomicModel (..)
      -- * CoupledModel
    , CoupledModel (..), Influencer (..), Influencers, SelfInfluencer (..), SelfInfluencers
      -- * ProcessorModel
    , ProcessorModel (..), ComponentRef(..)
      -- * References
      -- $references
    ) where

import Data.Binary
import Data.Set (Set)
import Data.IntMap
import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude
import Control.Distributed.Process.Serializable
import Data.DEVS.Simulation.Infinite

-- | The 'Time' type 'T' used in "Data.DEVS"
type T = Time Double

instance Binary T where
    put t = put $ t /~ second
    get = do t <- get
             return $ t *~ second


-- | the inifinite time
t_infinity :: T
t_infinity = infinity *~ second


data DeltaInt s = DeltaInt (s -> s)
data DeltaExt s x = DeltaExt (s -> T -> Set x -> s)
data DeltaCon s x = DeltaCon (s -> Set x -> s)
data Lambda s y = Lambda (s -> Set y)
data Ta s = Ta (s -> T)




-- | The /Parallel DEVS (P-DEVS)/ Model [PDEVS94].
class (Ord (X m), Serializable (X m), Ord (Y m), Serializable (Y m)) => Model m where
    type X m :: * 
    type Y m :: *
    data S m :: *

    -- | the models initial state
    s0 :: S m

    -- | the internal transition function
    delta_int :: m -> DeltaInt (S m)

    -- | the external transition function
    delta_ext :: m -> DeltaExt (S m) (X m)

    -- | the confluent transition function
    delta_con :: m -> DeltaCon (S m) (X m)

    -- | the output function of the model.
    lambda :: m -> Lambda (S m) (Y m)

    -- | the time advance function
    ta :: m -> Ta (S m)



-- | A 'PDEVS' model which does not have furter 'ComponentRef's
class (Model m) => AtomicModel m where

-- | A 'PDEVS' model which is composed out of other 'AtomicModel' or 'CoupledModel'
class (Model m) => CoupledModel m where
    -- | the map of influencers, for all components in 'componentRefs'
    influencers :: m -> Influencers (X m)
                      
    -- | the influencers of the model it self
    selfInfluencers :: m -> SelfInfluencers (X m) (Y m)
    

data SelfInfluencer x y = forall jx jy . SelfInfluencer 
     ( ComponentRef jx jy
     , Either (jy -> y) (jy -> x)
     )
type SelfInfluencers x y = Set (SelfInfluencer x y)

data Influencer x = forall ix iy . Influencer
    ( ComponentRef ix iy
    , (forall jx jy . Set (ComponentRef jx jy, Either (x -> ix) (jy -> ix)))
    )

type Influencers x = IntMap (Influencer x)

data ComponentRef x y = forall s . Ref (ProcessorModel s x y)

data ProcessorModel s x y where
    SimulatorModel :: DeltaInt s 
                -> DeltaExt s x 
                -> DeltaCon s x
                -> Lambda s y 
                -> Ta s 
                -> s
                -> ProcessorModel s x y
    CoordinatorModel :: Influencers x -> SelfInfluencers x y -> ProcessorModel () x y


-- $references
-- * [PDEVS94] Chow, A.C.; Zeigler, B.P., /Parallel DEVS: a parallel, hierarchical, modular modeling formalism/, Simulation Conference Proceedings, 1994. Winter, pp.716,722, 11-14 Dec. 1994, URL: <http://www.bgc-jena.mpg.de/~twutz/devsbridge/pub/chow96_parallelDEVS.pdf>




