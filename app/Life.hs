-- |
-- Module      : Main
-- Description : Yet another Conway's Game Of Life
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : $
-- Portability : $
--

-- TODO | - 
--        - 

-- GHC Pragmas -----------------------------------------------------------------

-- API -------------------------------------------------------------------------

module Main where

-- We'll need these ------------------------------------------------------------

import Graphics.UIKit as UIKit

-- Definitions -----------------------------------------------------------------

-- |
data Game = Game {

} deriving (Show)

-- |
renderLife :: Scene -> Game -> Image PixelRGBA8
renderLife scene game = _

-- |
updateLife :: SystemEvent -> Game -> Game
updateLife msg old = _

-- |
newLife :: Scene -> Input -> EitherT String IO ()
newLife scene initial = do
  lift $ animate (atomically . writeTChan (channel scene)) 30
  pure Game

-- |
runLife :: IO ()
runLife = UIKit.run $ Application {
  title      = "Game of Life",
  size       = V2 200 200,
  draw       = \scene game    -> renderLife scene game,
  update     = \msg old       -> updateLife msg old,
  initialise = \scene initial -> newLife scene initial
}

-- |
main :: IO ()
main = runLife >>= print