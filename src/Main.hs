module Main where

import ML4HS
import System.Environment

main = do args <- getArgs
          interact (run args)
