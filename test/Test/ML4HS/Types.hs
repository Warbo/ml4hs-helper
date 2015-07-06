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
    testProperty "Can parse ()"       parseUnit
  , testProperty "Can parse function" parseFunc
  ]

parseUnit = parse funType "()" == Just (L.List [L.String "()"])

parseFunc (NonEmpty ts) = parse funType typeStr == Just expected
  where ts'      = map (S.fromString . unTArg) ts
        typeStr  = T.intercalate " -> " ts'
        expected = L.List (map L.String ts')

inParens x  = "(" ++ stripParens x ++ ")"

stripParens = filter (not . (`elem` ("()"   :: String)))

stripType   = filter (not . (`elem` ("()->" :: String)))

parseSingle t = lLength result == 1
  where Just result = parse funType . S.fromString . inParens $ t

asString :: S.Stringable a => (String -> String) -> a -> a
asString f = S.fromString . f . S.toString
