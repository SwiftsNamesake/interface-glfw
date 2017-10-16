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
import Graphics.UIKit as UI

-- Definitions -----------------------------------------------------------------

-- |
main :: IO ()
main = runHanoi


-- |
runHanoi :: IO ()
runHanoi = runApplication
  (\scene game -> renderHanoi)
  (\msg old    -> updateHanoi)
  (\initial    -> newHanoi _)