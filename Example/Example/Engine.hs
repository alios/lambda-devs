{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Example.Engine where

import Data.DEVS
import Data.Binary
import Data.Typeable (Typeable)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude
import Numeric.NumType (Zero, Pos3, Neg1)

data Engine = Engine deriving (Typeable, Ord, Eq, Show)
instance PDEVS Engine where
    type Y Engine = Int
    type X Engine = Int
instance AtomicModel Engine
instance ProcessorModel Simulator Engine

pm_engine  = procModel Engine
ref_engine = selfRef Engine

