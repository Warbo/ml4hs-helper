{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List
import qualified Data.AttoLisp as L
import ML4HS
import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "typeCommand includes everything" typeCmdAll
  ]

typeCmdAll = forAll (listOf genAst) typeCmdAll'

typeCmdAll' ls = debug (("cmd", cmd)) $ ls `allIn` cmd
  where cmd            = typeCommand' ls
        allIn []     _ = True
        allIn (l:ls) s = ":t (" ++ getQName l ++ ")" `elem` lines s

debug = whenFail . print

genAst = do
  (P pkg, M mod, N name, ast) <- arbitrary
  -- We don't touch ASTs, so a String will do
  return $ L.List [
      L.List [
          lString "FOUNDAST"
        , lString pkg
        , lString mod
        , lString name
        ]
    , lString ast
    ]

lString :: String -> L.Lisp
lString = L.String . reString

-- Stringly-type our components

newtype Pkg  = P String deriving (Show)
newtype Mod  = M String deriving (Show)
newtype Name = N String deriving (Show)

(lower, upper) = (['a'..'z'], ['A'..'Z'])

instance Arbitrary Pkg where
  arbitrary = fmap P $ listOf1 (elements (lower ++ "-0123456789"))

instance Arbitrary Mod where
  arbitrary = fmap (M . intercalate ".") (listOf1 genBit)
    where genBit = do
            init <- elements upper
            rest <- listOf (elements (upper ++ lower))
            return (init:rest)

instance Arbitrary Name where
  arbitrary = do
    init <- elements lower
    rest <- listOf (elements (lower ++ upper ++ "'_"))
    return (N (init:rest))
