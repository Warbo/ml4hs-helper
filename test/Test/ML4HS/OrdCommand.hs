{-# LANGUAGE OverloadedStrings #-}

module Test.ML4HS.OrdCommand (tests) where

import qualified Data.Stringable as S
import qualified Data.Text       as T
import ML4HS
import Test.ML4HS.Utils
import Test.Tasty
import Test.Tasty.QuickCheck

tests = testGroup "ordCommand tests" [
    testProperty "ordLine handles arity" ordLineArity
  ]

ordLineArity = forAll genTypeLine check
  where check  l@(_, _, t) = argCount l == arity t
        argCount (m, n, t) = pred                  .
                             length                .
                             T.splitOn "undefined" .
                             ordLine               .
                             S.fromString          $
                             mkTypeLine m n t
