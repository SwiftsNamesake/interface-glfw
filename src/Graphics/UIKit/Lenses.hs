-- |
-- Module      : Graphics.UIKit.Lenses
-- Description : 
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : $
-- Portability : $
--

-- TODO | - 
--        - 

-- GHC Directives --------------------------------------------------------------

{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- API -------------------------------------------------------------------------

module Graphics.UIKit.Lenses where

-- We'll need these ------------------------------------------------------------

-- *
import Data.Set (Set)

-- *
import Lens.Micro.Platform
--import Lens.Micro.TH as TH
import Linear

-- *
import Data.AABB (AABB(..))

-- *
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW (MouseButton(..), Key(..), KeyState(..), MouseButtonState(..), CursorState(..))

-- *
import Graphics.UIKit.Types

-- Definitions ----------------------------------------------------------------

-- TODO | - Factor out

-- | A poor man's over
(~>) :: s -> SimpleGetter s a -> a
(~>) = (^.)

infixl 8 ~>


scroll :: Lens' Input (V2 Double)
scroll f s = (\new -> s { fScroll = new }) <$> f (fScroll s)

windowRect :: Lens' Input (AABB V2 Int)
windowRect f s = (\new -> s { fWindowRect = new }) <$> f (fWindowRect s)

frameSize :: Lens' Input (V2 Int)
frameSize f s = (\new -> s { fFrameSize = new }) <$> f (fFrameSize s)

mouse :: Lens' Input (Mouse)
mouse f s = (\new -> s { fMouse = new }) <$> f (fMouse s)

keyboard :: Lens' Input (Set Key)
keyboard f s = (\new -> s { fKeyboard = new }) <$> f (fKeyboard s)

time :: Lens' Input (Double)
time f s = (\new -> s { fTime = new }) <$> f (fTime s)

cursor  :: Lens' Mouse (V2 Double)
cursor f s = (\new -> s { fCursor = new }) <$> f (fCursor s)

buttons :: Lens' Mouse (Set MouseButton)
buttons f s = (\new -> s { fButtons = new }) <$> f (fButtons s)