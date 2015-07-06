{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import Test.ML4HS.OrdCommand  as OC
import Test.ML4HS.TypeCommand as TC
import Test.ML4HS.Types       as T
import Test.Tasty
import Test.Tasty.QuickCheck

main = defaultMain . localOption (QuickCheckTests 1) $ testGroup "All tests" [
    OC.tests
  , TC.tests
  , T.tests
  ]
