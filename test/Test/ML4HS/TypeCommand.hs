module Test.ML4HS.TypeCommand (tests) where

import ML4HS.Lisp
import ML4HS.TypeCommand
import Test.ML4HS.Utils
import Test.Tasty
import Test.Tasty.QuickCheck

tests = testGroup "typeCommand tests" [
    testProperty "typeCommand includes everything" typeCmdAll
  , testProperty "typeCommand unloads modules"     typeCmdMod
  ]

typeCmdAll = forAll (listOf genAst) typeCmdAll'

typeCmdAll' ls = debug (("cmd", cmd)) $ ls `allIn` cmd
  where cmd            = typeCommand' ls
        allIn []     _ = True
        allIn (l:ls) s = ":t (" ++ getQName l ++ ")" `elem` lines s

typeCmdMod = forAll (listOf genAst) hasMod
  where hasMod = (":m" ==) . head . lines . typeCommand'
