{-# LANGUAGE OverloadedStrings #-}
module ML4HS.RenderAsts where

import qualified Data.AttoLisp              as L
import qualified Data.Stringable            as S
import ML4HS.Lisp


renderAsts = unlines      .
            map show     .
            renderAsts'   .
            parseLisps   .
            S.fromString

renderAsts' ls = [giveType ast t | ast <- asts, t <- sigNamed (getQName ast)]
  where asts = filter isAst ls
        sigs = filter isSig ls
        sigName (L.List (L.String "::":L.String n:_)) = n
        sigNamed n = filter ((== n) . sigName) sigs


giveType line sig = cons id' (cdr line)
  where id  = nth 0 line
        id' = snoc (cdr id) typ  -- Drop "FOUNDAST" and append type
        typ = nth 2 sig

-- | (("FOUNDAST" ...) ...)
isAst (L.List (L.List (L.String "FOUNDAST":_):_)) = True
isAst _                                           = False

-- | ("::" "name" "type")
isSig (L.List (L.String "::":_)) = True
isSig _                          = False
