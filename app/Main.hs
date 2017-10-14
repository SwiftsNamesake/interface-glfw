module Main where

import Graphics.UIKit as UI

main :: IO ()
main = UI.runTicTacToe >>= either (putStrLn) (\_ -> putStrLn "Hurrah!")
