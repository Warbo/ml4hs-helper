{-# LANGUAGE OverloadedStrings #-}
module Test.ML4HS.OrdCommand where

import Data.List
import qualified Data.Stringable as S
import qualified Data.Text       as T
import ML4HS
import ML4HS.Types
import Test.ML4HS.Utils
import Test.Tasty
import Test.Tasty.QuickCheck

tests = testGroup "ordCommand tests" [
    testProperty "ordLine handles arity" ordLineArgCount
  ]

ordLineArgCount (NonEmpty ts) (QN n) = argCount == length ts
  where argCount = length                .
                   T.splitOn "undefined" .
                   ordLine               .
                   S.fromString          $
                   n ++ " :: " ++ intercalate " -> " (map unTArg ts)
