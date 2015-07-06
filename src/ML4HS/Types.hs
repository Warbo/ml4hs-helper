{-# LANGUAGE OverloadedStrings #-}
module ML4HS.Types where

import Control.Applicative
import qualified Data.Attoparsec.Text as AT
import qualified Data.AttoLisp        as L
import qualified Data.Stringable      as S
import qualified Data.Text            as T

parse p x = case AT.parseOnly (p <* AT.endOfInput) x of
  Left err -> Nothing
  Right x  -> Just x

-- | Parse Haskell types "a -> (b -> IO c) -> d" into ("a" ("b" "IO c") "d")
typeLisp s = case typeList s of
                  [t] -> L.toLisp [t]
                  ts  -> L.toLisp (map typeLisp ts)

-- | Turns "a -> (b -> c) -> d" into ["a", "(b -> c)", "d"]
typeList = reverse . map reverse . typeList' 0 [""] . unParen

-- | Parse one "level" of Haskell type syntax, eg. "a -> (b -> c) -> d" becomes
-- ["a", "b -> c", "d"]. In fact it's reversed: ["d", "c >- b", "a"]
typeList' :: Int -> [String] -> String -> [String]
typeList' _    ts  "" = ts                                 -- Base case
typeList' l (t:ts) s  = uncurry3 typeList' $ case s of
  '(':')':s'         -> (l,   ("()"++t):ts,           s')  -- Keep unit types
  '(':s'             -> (l+1, ('(':t):ts,             s')  -- Track nesting
  ')':s'             -> (l-1, (')':t):ts,             s')  -- Track nesting
  ' ':'-':'>':' ':s' -> (l,   if l == 0                    -- Are we at the top-level?
                                 then "":t:ts              -- Start a new entry
                                 else (" >- "++t):ts, s')  -- Remember the "->"
  c:s'               -> (l,   (c:t):ts,               s')  -- Remember any other Char

uncurry3 f (a, b, c) = f a b c

-- | Unwrap spurious parens "(((foo)))" into "foo"
unParen "" = ""
unParen x  = if head x == '(' && last x == ')'
                then unParen (init (tail x))
                else x

funType :: AT.Parser L.Lisp
funType = do ts <- (AT.skipSpace *> typeChunk <* AT.skipSpace) `AT.sepBy` "->"
             return (L.List ts)

typeChunk = AT.choice [unit, nested, typeName]

typeName = do x <- AT.takeWhile1 (AT.notInClass "()-")
              return (L.String (T.strip x))

nested = do AT.char '('
            ts <- funType
            AT.char ')'
            return ts

unit = AT.string "()" >> return (L.String "()")

bracketed = do AT.char '('
               s <- AT.manyTill (AT.choice [bracketed, aChar]) (AT.char ')')
               return (concat s)

aChar = do c <- AT.anyChar
           return [c]

takeBracketed = go "" 0
  where go a 1 (')':s) = (reverse (')':a), s)
        go a l ('(':s) = go ('(':a) (l+1) s
        go a l (')':s) = go (')':a) (l-1) s
        go a l   (c:s) = go   (c:a)  l    s
        go a l ""      = error ("Mismatched brackets: " ++ reverse a)
