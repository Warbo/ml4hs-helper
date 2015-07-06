module Test.ML4HS.Utils where

import Data.List
import qualified Data.AttoLisp   as L
import qualified Data.Stringable as S
import Test.QuickCheck

-- Stringly-type our components

newtype Pkg  = P  { unPkg  :: String } deriving (Show)
newtype Mod  = M  { unMod  :: String } deriving (Show)
newtype Name = N  { unName :: String } deriving (Show)
newtype Type = T  { unType :: String } deriving (Show)
newtype TArg = TA { unTArg :: String } deriving (Show)

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

instance Arbitrary TArg where
  arbitrary = do
    ts <- listOf1 arbitrary
    return . TA . unwords . map (\(M x) -> x) $ ts

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
genType = fmap (intercalate " -> " . map (\(M x) -> x))
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
