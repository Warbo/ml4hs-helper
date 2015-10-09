{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module ML4HS where

import Data.Maybe
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString      as B
import qualified Data.AttoLisp        as L
import qualified Data.Stringable      as S
import qualified Data.Text            as T

mkSig a n = let f = $(Test.QuickCheck.All.monomorphic n)
            in case n of
                    0 -> fun0 (Test.QuickCheck.All.monomorphic n)
                    1 -> fun1 f
                    2 -> fun2 f
                    3 -> fun3 f
                    4 -> fun4 f
                    5 -> fun5 f
