module Main (main) where

import           System.Environment (getArgs)
import           System.IO

import           LLifter.PlayGame
import           LLifter.OpenGL

main :: IO ()
main = do
    getArgs >>= (return . head) >>= openGLmain
    hSetBuffering stdin NoBuffering >> getArgs >>= runGame . head
