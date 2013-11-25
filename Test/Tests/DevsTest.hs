{-# LANGUAGE DeriveDataTypeable #-}

module Tests.DevsTest (devs_tests) where

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Data.Typeable (Typeable)
import qualified Prelude as P
import Numeric.Units.Dimensional.Prelude

import Data.DEVS

devs_tests :: Test.Framework.Test
devs_tests = testGroup "DEVS tests" 
             [ t_infinity_tests
             , selfRef_tests
             ]

-- 
-- t_infinity tests 
--
t_infinity_tests = testGroup "t_infinity_tests" 
                   [ testCase "t_infinity eq" test_t_infinity_eq 
                   , testProperty "t_infinity smaller" prop_t_infinity_smaller
                   ]
prop_t_infinity_smaller :: Double -> Property
prop_t_infinity_smaller d = 
    let dt = d *~ second
    in not (dt == t_infinity) ==> dt < t_infinity

test_t_infinity_eq :: Assertion
test_t_infinity_eq = t_infinity @?= t_infinity

--
-- selfRef tests
-- 
selfRef_tests = testGroup "t_infinity_tests" 
                [ testCase "selfRef eq 1" test_selfRef_eq1
                , testCase "selfRef eq 2" test_selfRef_eq2
                , testCase "selfRef neq 1" test_selfRef_neq1
                , testCase "selfRef neq 2" test_selfRef_neq2
                ]

data A1 = A1 deriving (Typeable, Show, Ord, Eq)
instance PDEVS A1
ref_a1 = selfRef A1 

data A2 = A2 deriving (Typeable, Show, Ord, Eq)
instance PDEVS A2
ref_a2 = selfRef A2

test_selfRef_eq1 = ref_a1 @?= ref_a1
test_selfRef_eq2 = ref_a2 @?= ref_a2
test_selfRef_neq1 = assertBool (show ref_a1 ++ " must not be equal to " ++ show ref_a2 ) $ 
                    ref_a1 /= ref_a2
test_selfRef_neq2 = assertBool (show ref_a1 ++ " must not be equal to " ++ show ref_a2 ) $ 
                    ref_a2 /= ref_a1


