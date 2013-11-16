\begin{code}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.DEVS.Devs (T, DEVS (..), CoupledModel (..)) where

import Data.Binary
import Data.Typeable (Typeable)
import Data.Set (Set)

\end{code}
\begin{code}


type T = Double

class (Typeable m, Ord (X m), Binary (X m), Binary (Y m)) => DEVS m where
    data S m :: *
    type X m :: *
    type Y m :: *

    s0 :: m -> S m
    lambda :: m -> S m -> Y m
    delta_ext :: m -> S m -> T -> Set (X m) -> S m
    delta_int :: m -> S m -> S m
    delta_con :: m -> S m -> Set (X m) -> S m
    ta :: m -> S m -> T
 
class (DEVS m) => CoupledModel m where
    data CX m :: *
    data CY m :: *
             


\end{code}
