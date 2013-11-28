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
{-# LANGUAGE ExistentialQuantification #-}


-- | The /Discrete Event System Sepcification (DEVS)/ formalism defines
--   discrete event simulation models in a hierachical, modular manner.
module Data.DEVS.Devs 
    ( -- * Parallel DEVS
      PDEVS (..)
      -- * Time base 
    , T, t_infinity
      -- * AtomicModel  
    , AtomicModel (..)
      -- * CoupledModel
    , CoupledModel (..), ComponentRef, Z (..)
      -- * References
      -- $references
    ) where

import Data.Binary
import Data.Typeable (Typeable, cast)
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (isJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude
import Data.DEVS.Simulation.Infinite

-- | The 'Time' type 'T' used in "Data.DEVS"
type T = Time Double

-- | the inifinite time
t_infinity :: T
t_infinity = infinity *~ second

-- | The /Parallel DEVS (P-DEVS)/ Model [PDEVS94].
class (Typeable m, Ord m, Show m) => PDEVS m where
    type X m :: * 
    type Y m :: *
    data S m :: *

    -- | the models initial state
    s0 :: m -> S m

    -- | the internal transition function
    delta_int :: m -- ^ the model
              -> S m -- ^ the current state
              -> S m -- ^ the new state

    -- | the external transition function
    delta_ext :: m -- ^ the model 
              -> S m -- ^ the current state
              -> T -- ^ the current time
              -> Set (X m) -- ^ the set of input events at current time
              -> S m -- ^ the new state

    -- | the confluent transition function
    delta_con :: m -- ^ the model 
              -> S m -- ^ the current state
              -> Set (X m) -- ^ the set of input events at current time
              -> S m -- ^ the new state

    -- | the output function of the model.
    lambda :: m -- ^ the model 
           -> S m -- ^ the current state
           -> Y m -- ^ the output

    -- | the time advance function
    ta :: m -- ^ the model 
       -> S m -- ^ the current state
       -> T -- ^ the time of next internal event

    -- | a reference to the model it self as a component of a 'CoupledModel'.
    selfRef :: m -> ComponentRef
    selfRef m = MkComponentRef m


-- | A 'PDEVS' model which does not have furter 'ComponentRef's
class (PDEVS m) => AtomicModel m

-- | A 'PDEVS' model which is composed out of other 'AtomicModel' or 'CoupledModel'
class (PDEVS d) => CoupledModel d where
    -- | the set of 'CoupledModel's components
    componentRefs ::  d -> Set ComponentRef

    -- | the map of influencers, for all components in 'componentRefs'
    compInfluencers :: d -> Map ComponentRef (Set ComponentRef)
                      
    -- | the influencers of the model it self
    selfInfluencers :: d -> Set ComponentRef
    
    -- | the map of influencers, for all components in 'componentRefs' and self
    influencers :: d -> Map ComponentRef (Set ComponentRef)
    influencers d =
        let crefs' = compInfluencers d
            crefs_no_self = case (Map.lookup (selfRef d) crefs') of
                              Nothing -> crefs'
                              Just _ -> error $ "compInfluencers of " ++ show d ++ 
                                       " must not contain a reference to the model it self"
            x = Set.map f $ componentRefs d
            f r = isJust $ Map.lookup r $ crefs'
            y = Set.fold (&&) True x
            crefs = if (y) then crefs_no_self else 
                        error $ "compInfluencers of " ++ show d ++ " must contain an entry " ++
                              "for every Component in componentRefs"
            allinfs = Map.insert (selfRef d) (selfInfluencers d) crefs
        in allinfs
    
-- | a Reference to a 'PDEVS' component hiding its model
data ComponentRef = forall m . (PDEVS m) => MkComponentRef m

-- | a @i@-to-@j@ output translation used for coupleing the components of a 'CoupledModel'.
--
--  [@ExtCoup@] external coupling (translate 'CoupledModel' @i@ input to component @j@ input)
--
--  [@IntCoup@] internal coupling (translate component @i@ output to component @j@ input)
--
--  [@OutCoup@] internal coupling (translate component @i@ output to 'CoupledModel' @j@ output)
data Z i j where
    ExtCoup :: ((X i) -> (X j)) 
            -> Z i j
    IntCoup :: ((Y i) -> (X j))  
            -> Z i j
    OutCoup :: ((Y i) -> (Y j)) 
            -> Z i j



-- $references
-- * [PDEVS94] Chow, A.C.; Zeigler, B.P., /Parallel DEVS: a parallel, hierarchical, modular modeling formalism/, Simulation Conference Proceedings, 1994. Winter, pp.716,722, 11-14 Dec. 1994, URL: <http://www.bgc-jena.mpg.de/~twutz/devsbridge/pub/chow96_parallelDEVS.pdf>



instance Eq (ComponentRef) where
    (MkComponentRef m1) == (MkComponentRef m2) =
        case (cast m2) of
          Nothing -> False
          Just m2' -> m1 == m2'

instance Ord (ComponentRef) where
    compare (MkComponentRef m1) (MkComponentRef m2) =
        case (cast m2) of 
          Nothing -> compare (show m1) (show m2)
          Just m2' -> compare m1 m2'

instance Show (ComponentRef) where
    show (MkComponentRef m) = "MkComponentRef (" ++ show m  ++ ")"

instance Binary T where
    put = (put :: Time Double -> Put)
    get = (get :: Get (Time Double))



