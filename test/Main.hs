{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import Data.List
import qualified Data.AttoLisp as L
import qualified Data.Text     as T
import ML4HS
import Test.QuickCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

main = defaultMain $ testGroup "All tests" [
    testProperty "typeCommand includes everything" typeCmdAll
  , testProperty "typeCommand unloads modules"     typeCmdMod
  ]

typeCmdAll = forAll (listOf genAst) typeCmdAll'

typeCmdAll' ls = debug (("cmd", cmd)) $ ls `allIn` cmd
  where cmd            = typeCommand' ls
        allIn []     _ = True
        allIn (l:ls) s = ":t (" ++ getQName l ++ ")" `elem` lines s

typeCmdMod = forAll (listOf genAst) hasMod
  where hasMod = (":m" ==) . head . lines . typeCommand'

ordLineArity = forAll genTypeLine ordLineArity'

ordLineArity' :: (String, String, String) -> Bool
ordLineArity' (m, n, t) = argCount == arity t
  where argCount = pred                  .
                   length                .
                   T.splitOn "undefined" .
                   reString              .
                   ordLine               .
                   reString              $
                   mkTypeLine m n t

-- Helpers

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

-- Type names look like module names. Feel free to make this more exhaustive.
genType = fmap (intercalate " -> " . map (reString . (\(M x) -> x)))
               (listOf1 arbitrary)

genTypeLine = do
  (M m, N n) <- arbitrary
  t <- genType
  return (m, n, t)

mkTypeLine m n t = concat ["(", mkQName m n, ") :: ", t]
mkQName m n = concat [m, ".", n]

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
