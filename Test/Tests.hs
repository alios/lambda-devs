module Main (main) where

import Data.Monoid
import Test.Framework

import Tests.DevsTest

main :: IO ()
main = defaultMainWithOpts
       [ devs_tests
       ] mempty
