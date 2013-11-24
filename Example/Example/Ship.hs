{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Example.Ship where

import Data.DEVS
import Data.Binary
import Data.Typeable (Typeable)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Prelude as P
import qualified Data.Map as Map


import Numeric.Units.Dimensional.Prelude
import Numeric.NumType (Zero, Pos3, Neg1)

import Example.Tank
import Example.Engine

data Ship = Ship deriving (Typeable, Ord, Eq, Show)
instance PDEVS Ship where
    type Y Ship = Maybe String
    type X Ship = Maybe String
instance ProcessorModel Coordinator Ship
instance CoupledModel Ship where
    componentRefs m = Set.fromList [ref_tank, ref_engine]
    compInfluencers m = Map.fromList [
                          (ref_tank, Set.fromList [ref_engine]),
                          (ref_engine, Set.fromList [ref_tank, ref_ship])
                         ]
    selfInfluencers m = Set.fromList []

pm_ship = procModel Ship
ref_ship = selfRef Ship
