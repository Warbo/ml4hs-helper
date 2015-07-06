{-# LANGUAGE OverloadedStrings #-}
module ML4HS.Lisp where

import qualified Data.AttoLisp              as L
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.ByteString.Char8      as C
import qualified Data.Stringable            as S
import Data.Maybe

-- | Working with s-expressions

parseLisps = mapMaybe parseLisp . C.split '\n'
parseLisp  = AB.maybeResult . AB.parse L.lisp

nth  i (L.List xs)   = xs !! i
car                  = nth 0
cdr    (L.List xs)   = L.List (tail xs)
cons x (L.List xs)   = L.List (x:xs)
snoc   (L.List xs) x = L.List (xs ++ [x])

unString (L.String x) = S.toString x

lLength (L.List xs) = length xs

-- | Input has the form (id ast), where id may have many components

getId   = nth 0
getMod  = unString . nth 2
getName = unString . nth 3

getQName :: S.Stringable a => L.Lisp -> a
getQName x = S.fromString $ concat [m, ".", n]
  where id = getId   x
        m  = getMod  id
        n  = getName id
