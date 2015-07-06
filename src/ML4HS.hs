{-# LANGUAGE OverloadedStrings #-}
module ML4HS where

import Data.Maybe
import qualified Data.Attoparsec.Text       as AT
import qualified Data.ByteString            as B
import qualified Data.AttoLisp              as L
import qualified Data.Stringable            as S
import qualified Data.Text                  as T
import ML4HS.RenderAsts
import ML4HS.TypeCommand
import ML4HS.Types

-- To minimise headaches, we try to use s-expressions for data interchange.
-- We use types, parsers and pretty-printers from AttoLisp, which forces us to
-- juggle Strings, ByteStrings and Text using toString and fromString.

run :: [String] -> String -> String
run args stdin = cmd stdin
  where cmd :: String -> String
        cmd = case args of
          ["typeCommand"] -> typeCommand
          ["ordCommand"]  -> ordCommand
          ["renderAsts"]  -> renderAsts
          _               -> error $ "Unknown arguments '" ++ show args ++ "'"

-- Try to partially-apply ">" to each value, to see if it admits an instance
-- of Ord
ordCommand = S.toString  .
             T.unlines   .
             (":m":)     .
             map ordLine .
             T.lines     .
             S.fromString

ordLine x = T.concat [":t (", applyTo ">" result, ")"]
  where [name, typeStr] = T.splitOn " :: " x
        result = addArgs name (arity typeStr)

arity :: S.Stringable a => a -> Int
arity = arity' . typeLisp . S.toString
  where arity' (L.List xs) = 1 - length xs

applyTo x y = T.concat ["(", x, ") (", y, ")"]

addArgs x 0 = x
addArgs x n = addArgs (applyTo x "undefined") (n-1)
