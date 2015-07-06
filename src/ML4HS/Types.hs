{-# LANGUAGE OverloadedStrings #-}
module ML4HS.Types where

import Control.Applicative
import qualified Data.Attoparsec.Text as AT
import qualified Data.AttoLisp        as L
import qualified Data.Stringable      as S
import qualified Data.Text            as T

-- Parsing Haskell type signatures

uParse p x = let Just result = parse p x
              in result

parse p x = case AT.parseOnly (p <* AT.endOfInput) x of
  Left err -> Nothing
  Right x  -> Just x

-- | Haskell function types
funType = L.List <$> ((AT.skipSpace *> typeChunk <* AT.skipSpace) `AT.sepBy` "->")

-- | Argument or return types
typeChunk = AT.choice [unit, nested, typeName]

-- | Everything up to "-" (ie. "->"), ")" or "("
typeName = L.String . T.strip <$> AT.takeWhile1 (AT.notInClass "()-")

-- | Types in (parentheses). Use this after `unit`, not before.
nested = AT.char '(' *> funType <* AT.char ')'

-- | "()". Use this before `nested`, not after.
unit = L.String <$> "()"

-- Manipulating type signatures

arity :: T.Text -> Int
arity = arity' . parse funType
  where arity' (Just (L.List xs)) = length xs - 1
