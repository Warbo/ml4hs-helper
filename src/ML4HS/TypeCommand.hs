{-# LANGUAGE OverloadedStrings #-}
module ML4HS.TypeCommand where

import qualified Data.Stringable            as S
import qualified Data.Text                  as T
import HS2AST.Types
import ML4HS.Lisp

-- Build a command for GHCi to inspect the type of each name
-- The ":m" unloads everything except Prelude, ensuring that names all get
-- qualified and we can't see any non-exported members
typeCommand = typeCommand' .
              parseLisps   .
              S.fromString

typeCommand' = S.toString . T.unlines . (":m":) . map cmd
  where cmd s = T.concat [":t (", getQName s, ")"]
