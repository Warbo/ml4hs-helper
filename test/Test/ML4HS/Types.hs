{-# LANGUAGE OverloadedStrings #-}
module Test.ML4HS.Types where

import qualified Data.Stringable as S
import ML4HS.Lisp
import ML4HS.Types
import Test.Tasty
import Test.Tasty.QuickCheck

tests = testGroup "Type tests" [
    testProperty "Can parse (parentheses)" parseBracketed1
  ]

parseBracketed1 :: String -> String -> Bool
parseBracketed1 inner after = result == (inner', after)
  where inner' = inParens inner
        result = takeBracketed (inner' ++ after)

parseBracketedN = forAll (infiniteListOf arbitrary) parseBracketedN'

parseBracketedN' :: [Bool] -> [String] -> String -> Bool
parseBracketedN' bs ss after = result == (made, after)
  where make a  b         []     = a
        make a  (True:bs) (x:xs) = make (a ++ inParens x)                  bs xs
        make a (False:bs) (x:xs) = make ("(" ++ a ++ ")" ++ stripParens x) bs xs
        made   = make "" bs ss
        result = takeBracketed (made ++ after)

inParens x = "(" ++ stripParens x ++ ")"
stripParens = filter (not . (`elem` ("()" :: String)))

parseSingle t = lLength result == 1
  where Just result = parse funType . S.fromString . inParens $ t
