{-# LANGUAGE OverloadedStrings #-}
module ML4HS.Types where

import Control.Applicative
import qualified Data.Attoparsec.Text as AT
import qualified Data.AttoLisp        as L
import qualified Data.Text            as T

-- Parsing Haskell type signatures

parse p x = case AT.parseOnly (p <* AT.endOfInput) x of
  Left err -> Nothing
  Right x  -> Just x

-- | Haskell function types
funType = L.List <$> (typeChunk `AT.sepBy` "->")

-- | Argument or return types
typeChunk = trim (AT.choice [unit, nested, typeName])

-- | Everything up to "(", ")" or "-" (ie. "->")
typeName = L.String . T.strip <$> AT.takeWhile1 (AT.notInClass "()-")

-- | Types in (parentheses). Use this after `unit`, not before.
nested = AT.char '(' *> funType <* AT.char ')'

-- | "()". Use this before `nested`, not after.
unit = L.String <$> "()"

trim p = AT.skipSpace *> p <* AT.skipSpace

-- Manipulating type signatures

arity :: T.Text -> Maybe Int
arity = arity' . parse funType
  where arity' (Just (L.List xs)) = Just (length xs - 1)
        arity' _                  = Nothing
