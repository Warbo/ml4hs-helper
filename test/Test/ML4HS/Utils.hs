{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.ML4HS.Utils where

import Data.List
import Data.String
import qualified Data.AttoLisp   as L
import qualified Data.Stringable as S
import Test.QuickCheck

-- Stringly-type our components

reString :: (S.Stringable a, S.Stringable b) => a -> b
reString = S.fromString . S.toString

instance S.Stringable L.Lisp where
  toString (L.String x) = S.toString x
  fromString = fromString
  length = length . S.toString

newtype Pkg   = P  { unPkg   :: String } deriving (Show, IsString, S.Stringable)
newtype Mod   = M  { unMod   :: String } deriving (Show, IsString, S.Stringable)
newtype Name  = N  { unName  :: String } deriving (Show, IsString, S.Stringable)
newtype Type  = T  { unType  :: String } deriving (Show, IsString, S.Stringable)
newtype TArg  = TA { unTArg  :: String } deriving (Show, IsString, S.Stringable)
newtype QName = QN { unQName :: String } deriving (Show, IsString, S.Stringable)

instance Arbitrary Pkg where
  arbitrary = P <$> listOf1 (elements (lower ++ "-0123456789"))

instance Arbitrary Mod where
  arbitrary = fmap (M . intercalate "." . take 5) (listOf1 genBit)
    where genBit = do
            init <- elements upper
            rest <- listOf (elements (upper ++ lower))
            return (init:rest)

instance Arbitrary Name where
  arbitrary = do
    init <- elements lower
    rest <- listOf (elements (lower ++ upper ++ "'_"))
    return (N (init:rest))

instance Arbitrary TArg where
  arbitrary = do
    ts <- listOf1 arbitrary
    return . TA . unwords . map unMod . take 3 $ ts

instance Arbitrary QName where
  arbitrary = do
    (M m, N n) <- arbitrary
    return (QN (m ++ "." ++ n))

-- Custom generators

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
genType = fmap (intercalate " -> " . map unMod)
               (listOf1 arbitrary)

genTypeLine = do
  (M m, N n) <- arbitrary
  t          <- genType
  return (M m, N n, t)

-- Pure data handling

(lower, upper) = (['a'..'z'], ['A'..'Z'])

lString :: String -> L.Lisp
lString = L.String . S.fromString

mkTypeLine m     n t = concat ["(", mkQName m n, ") :: ", t]
mkQName (M m) (N n)  = concat [m, ".", n]

-- Testing utilities

debug :: (Show a, Testable b) => a -> b -> Property
debug = whenFail . print
