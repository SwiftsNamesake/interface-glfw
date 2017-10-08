module Main where

import Graphics.UIKit as UI

main :: IO ()
main = UI.run >>= either (putStrLn) (\_ -> putStrLn "Hurrah!")
