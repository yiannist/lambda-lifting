module Main (main) where

import           System.Environment (getArgs)
import           System.IO

import           LLifter.PlayGame
import           LLifter.OpenGL

main :: IO ()
main = do
    hSetEcho stdin True
    hSetBuffering stdin NoBuffering
    getArgs >>= LLifter.OpenGL.runGame . head
    getArgs >>= LLifter.PlayGame.runGame . head
