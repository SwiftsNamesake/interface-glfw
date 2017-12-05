-- |
-- Module      : Main
-- Description : Towers of Hanoi
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : $
-- Portability : $
--

-- TODO | - 
--        - 

-- API -------------------------------------------------------------------------

module Main where

-- We'll need these ------------------------------------------------------------

-- *
import Graphics.UIKit as UIKit

-- Definitions -----------------------------------------------------------------

-- |
runHanoi :: IO ()
runHanoi = UIKit.run $ Application {
  title  = "Towers of Hanoi",
  size   = V2 200 200,
  draw   = \scene game -> renderHanoi,
  update = \msg old    -> updateHanoi,
  initialise = \initial -> newHanoi _
}

-- |
main :: IO ()
main = runHanoi