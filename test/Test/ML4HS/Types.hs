{-# LANGUAGE OverloadedStrings #-}
module Test.ML4HS.Types where

import qualified Data.AttoLisp   as L
import qualified Data.Stringable as S
import qualified Data.Text       as T
import ML4HS.Lisp
import ML4HS.Types
import Test.ML4HS.Utils
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

tests = testGroup "Type tests" [
    testProperty "Can parse ()"        parseUnit
  , testProperty "Can parse function"  parseFunc
  , testProperty "(Parentheses) group" parseParens
  , testProperty "Arity is correct"    arityMatches
  ]

parseUnit = uParse funType "()" == L.List [L.String "()"]

parseFunc :: NonEmptyList TArg -> Bool
parseFunc (NonEmpty ts) = uParse funType typeStr == expected
  where typeStr  = mkFunType ts
        expected = L.List (map reString ts)

parseParens :: NonEmptyList (NonEmptyList TArg) -> Bool
parseParens (NonEmpty ts) = lLength (uParse funType (typeStr ts')) == length ts'
  where ts'                   = take 10 ts
        mkChunk (NonEmpty xs) = T.concat ["(", mkFunType (take 10 xs), ")"]
        typeStr               = mkFunType . map mkChunk

arityMatches :: NonEmptyList TArg -> Bool
arityMatches (NonEmpty ts) = arity (mkFunType ts) == Just (length ts - 1)

-- Helpers

inParens x  = "(" ++ stripParens x ++ ")"

stripParens = filter (not . (`elem` ("()"   :: String)))

stripType   = filter (not . (`elem` ("()->" :: String)))

asString :: S.Stringable a => (String -> String) -> a -> a
asString f = S.fromString . f . S.toString

mkFunType :: S.Stringable a => [a] -> T.Text
mkFunType = T.intercalate " -> " . map reString

uParse p x = let Just result = parse p x
              in result
