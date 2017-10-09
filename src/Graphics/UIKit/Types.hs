-- |
-- Module      : Graphics.UIKit.Types
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

module Graphics.UIKit.Types where

-- We'll need these ------------------------------------------------------------

-- *
import Data.Set (Set)

import Linear

import Control.Concurrent.STM

import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW (MouseButton(..), Key(..), KeyState(..), MouseButtonState(..), CursorState(..))

-- *
import Data.AABB(AABB(..))

-- Definitions ----------------------------------------------------------------

-- |
-- TODO | - Timestamps
--        -
-- newtype InputChannel = InputChannel (TChan SystemEvent)
type MessageChannel = TChan SystemEvent

-- |
-- TODO | - Complete (resize, minimise, maximise, enter, leave, Pending FileDrop, etc.)
--        - Elm style event mapping (eg. UIEvent -> AppAction) (?)
--        - Rename (?)
data SystemEvent = MouseMotion (V2 Double)
                 | MouseDown   MouseButton
                 | MouseUp     MouseButton
                 | MouseScroll (V2 Double)
                 | KeyDown     Key
                 | KeyUp       Key
                 | KeyRepeat   Key
                 | FileDrop    [String]
                 -- | FileChanged () -- TODO: Fix
                 | WindowClosing
                 | MouseEnter
                 | MouseLeave
                 deriving (Eq, Show)


-- |
-- TODO | - Rename, or break up
data Input = Input {
  -- fBounds   :: AABB V2 Int,
  --fWindow       :: GLFW.Window,
  --fRespond      :: app -> SystemEvent -> IO app,
  fScroll       :: V2 Double,
  -- TODO | - Framebuffer (client), or entire window
  --        - Rename
  --        - Position of framebuffer w.r.t. the window
  fWindowRect   :: AABB V2 Int,
  fFrameSize    :: V2 Int,
  fMouse        :: Mouse,
  fKeyboard     :: Set Key,
  fTime         :: Double -- Current time (updated on each `tick`)
  --fMessages     :: MessageChannel
} -- deriving (Show)


-- |
data Mouse = Mouse {
  fCursor  :: V2 Double,
  fButtons :: Set MouseButton
} deriving (Show)

-- Graphics --------------------------------------------------------------------

-- |
data VAODescriptor = VAODescriptor {
  vao   :: !GL.VertexArrayObject, -- * A reference to the VAO itself
  start :: !GL.ArrayIndex,        -- * The starting index
  count :: !GL.NumArrayIndices    -- * The number of elements
} deriving (Eq, Show)


-- |
data AttributeDescriptor = AttributeDescriptor {
  buffer :: !GL.BufferObject, -- * A reference to the buffer object itself
  count  :: !GL.GLsizeiptr,   -- * The number of elements
  elementSize :: !Int         -- * The number of components for each element
} deriving (Eq, Show)
