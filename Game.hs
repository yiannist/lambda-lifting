module Main (main) where

import           System.Environment (getArgs)
import           System.IO

import           LLifter.PlayGame

main :: IO ()
main = hSetBuffering stdin NoBuffering >> getArgs >>= runGame . head
