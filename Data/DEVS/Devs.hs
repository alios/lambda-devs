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

module Data.DEVS.Devs 
    (T, t_infinity
    , Model (..)
    , AtomicModel (..)
    , CoupledModel (..), ComponentRef (..), Z (..)
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

type T = Time Double
t_infinity :: T
t_infinity = infinity *~ second


class (Typeable m, Ord m, Show m) => Model m where
    type X m :: *
    type Y m :: *
    data S m :: *

    s0 :: m -> S m
    selfRef :: m -> ComponentRef
    selfRef m = MkComponentRef m
    lambda :: m -> S m -> Y m
    delta_ext :: m -> S m -> T -> Set (X m) -> S m
    delta_int :: m -> S m -> S m
    delta_con :: m -> S m -> Set (X m) -> S m
    ta :: m -> S m -> T

class (Model m) => AtomicModel m


class (Model d) => CoupledModel d where
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
    

data ComponentRef = forall m . (Model m) => MkComponentRef m

data Z i j where
    ExtCoup :: ((X i) -> (X j)) -> Z i j
    IntCoup :: ((Y i) -> (X j)) -> Z i j
    OutCoup :: ((Y i) -> (Y j)) -> Z i j


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



