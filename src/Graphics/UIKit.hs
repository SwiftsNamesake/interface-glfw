-- |
-- Module      : Graphics.UIKit
-- Description : 
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : 
-- Portability : Portable
-- 

-- TODO | - More powerful event churning (frp, elm, reactive) 
--        - 

-- GHC Directives --------------------------------------------------------------

-- API -------------------------------------------------------------------------

module Graphics.UIKit where
  
-- We'll need these ------------------------------------------------------------

import           Data.AABB as AABB
import           Data.Foldable
import qualified Data.Set as Set
import           Data.Set (Set)

import Control.Monad.Trans.Either
import Control.Concurrent.STM

import Linear
import Lens.Micro

import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW (MouseButton(..), Key(..), KeyState(..), MouseButtonState(..), CursorState(..))

-- Definitions -----------------------------------------------------------------

-- |
-- TODO | - Timestamps
--        -
-- newtype InputChannel = InputChannel (TChan SystemEvent)
type MessageChannel = TChan SystemEvent

-- |
-- TODO | - Complete (resize, minimise, maximise, enter, leave, Pending FileDrop, etc.)
--        - Elm style event mapping (eg. UIEvent -> AppAction) (?)
--        - Rename (?)
data SystemEvent = MouseMotion (V2 Float)
                 | MouseDown   MouseButton
                 | MouseUp     MouseButton
                 | MouseScroll (V2 Float)
                 | KeyDown     Key
                 | KeyUp       Key
                 | KeyRepeat   Key
                 | FileDrop    [String]
                 -- | FileChanged () -- TODO: Fix
                 | WindowClosing
                 deriving (Eq, Show)

-- |
-- TODO | - Rename, or break up
data Input app = Input {
  -- fBounds   :: AABB V2 Int,
  fWindow       :: GLFW.Window,
  fRespond      :: app -> SystemEvent -> IO app,
  fScroll       :: V2 Float,
  -- TODO | - Framebuffer (client), or entire window
  --        - Rename
  --        - Position of framebuffer w.r.t. the window
  fWindowRect   :: AABB V2 Int,
  fFrameSize    :: V2 Int,
  fMouse        :: Mouse,
  fKeyboard     :: Set Key,
  fTime         :: Float, -- Current time (updated on each `tick`)
  fMessages     :: MessageChannel
} -- deriving (Show)

-- |
data Mouse = Mouse {
  fCursor  :: V2 Float,
  fButtons :: Set MouseButton
} deriving (Show)

--------------------------------------------------------------------------------

-- TODO | - Factor out

-- | A poor man's over
(~>) :: a -> (a -> b) -> b
(~>) x f = f x


-- | Promote a 'Bool' to an 'EitherT'
insist :: Monad m => e -> Bool -> EitherT e m ()
insist e False = left e
insist _ True  = right ()


-- | Promote a 'Maybe' to an 'EitherT'
-- TODO | - Factor out
explain :: Monad m => e -> Maybe a -> EitherT e m a
explain e = maybe (left e) right


-- | Do nothing whatsoever
pass :: Applicative f => f ()
pass = pure ()

--------------------------------------------------------------------------------

-- TODO | - Factor out

-- |
whileM :: Monad m => m Bool -> m a -> m ()
whileM p act = p >>= \yes -> if yes then act >> whileM p act else pass


-- |
untilM :: Monad m => m Bool -> m a -> m ()
untilM p act = whileM (not <$> p) act


-- |
iterateWhileM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
iterateWhileM p act x = p x >>= \yes -> if yes then act x >>= iterateWhileM p act else return x


-- |
iterateUntilM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
iterateUntilM p act x = iterateWhileM (fmap not . p) act x

--------------------------------------------------------------------------------

-- |
vectorise :: Functor f => f (a,a) -> f (V2 a)
vectorise = fmap (uncurry V2)

--------------------------------------------------------------------------------

-- |
-- TODO | - Make sure there are no off-by-one errors
--          (how should coords be interpreted: inclusive or exclusive)
windowBounds :: GLFW.Window -> IO (AABB V2 Int)
windowBounds win = AABB.fromCornerSize
                     <$> vectorise (GLFW.getWindowPos win)
                     <*> vectorise (GLFW.getWindowSize win)


-- |
-- TODO | - Rename (?)
frameSize :: GLFW.Window -> IO (V2 Int)
frameSize win = vectorise (GLFW.getFrameBufferSize win)

--------------------------------------------------------------------------------

-- |
setup :: V2 Int -> EitherT String IO Input
setup (V2 dx dy) = do
  insist "Failed to initialise GLFW" =<< lift GLFW.init
  win <- explain "Failed to create window" =<< lift (GLFW.createWindow dx dy "Visual Overlap" Nothing Nothing)
  lift . GLFW.makeContextCurrent $ Just win
  lift $ setupEvents win


-- |
setupEvents :: GLFW.Window -> IO Input
setupEvents win = do
  chan <- newTChanIO
  let put = atomically . writeTChan chan
  
  GLFW.setCursorPosCallback   win (Just $ \win mx my        -> put $ MouseMotion (V2 mx my))
  GLFW.setCursorEnterCallback win (Just $ \win st           -> put $ makeFocus st)
  GLFW.setMouseButtonCallback win (Just $ \win b st mod     -> put $ makeMouse b st mod)
  GLFW.setKeyCallback         win (Just $ \win key r st mod -> put $ makeKey k r st mod)
  GLFW.setDropCallback        win (Just $ \win fns          -> put $ FileDrop fns)
  -- GLFW.setFramebufferSizeCallback win (Just $ \win dx dy -> put $ V2 _ _)
  -- GLFW.setWindowSizeCallback  win (Just $ \win dx dy -> put $ V2 _ _)
  --GLFW.setErrorCallback $ Just (\err s -> _)
  -- setClipboardString, getClipboardString
  (Just t) <- GLFW.getTime
  cur      <- uncurry V2 <$> GLFW.getCursorPos win
  wrect    <- windowBounds win
  
  return $ Input {
    fMessages   = chan,
    fTime       = t,
    fWindowRect = wrect,
    fMouse      = Mouse cur Set.empty,
    fKeyboard   = Set.empty }
  where
    makeFocus CursorState'InWindow = MouseEnter
    makeFocus _                    = MouseLeave

    makeMouse b MouseButtonState'Pressed  _ = MouseDown b
    makeMouse b MouseButtonState'Released _ = MouseUp b

    makeKey k _ (KeyState'Pressed)   mods = KeyDown k
    makeKey k _ (KeyState'Released)  mods = KeyUp k
    makeKey k _ (KeyState'Repeating) mods = KeyRepeat k

-- |
-- TODO | - How do we deal with repeat events (does it even matter)?
--        - We'll probably want to replace this logic when we move on to proper FRP
processEvents :: Input -> (Input -> s -> SystemEvent -> IO s) -> s -> IO s
processEvents bundle dispatch s = do
  events <- liftIO $ whileJust (atomically . tryReadTChan $ fMessages input) return
  wrect  <- windowBounds (bundle~>fWindow)
  fsz    <- frameSize    (bundle~>fWindow)
  Just t <- GLFW.getTime -- TODO: Fix this
  let bundle' = bundle { fFrameSize  = fsz,
                         fWindowRect = wrect,
                         fTime       = realToFrac t }
      bundle'' = foldr (flip onevent) bundle' events

-- Events --------------------------------------------------------------------------------------------------------------------------------------------

-- | Updates the 'Input', given a 'SystemEvent'
-- TODO | - 
onevent :: SystemEvent -> Input -> Input
onevent e = case e of
  MouseMotion pos' -> mouse.cursor .~ pos
  KeyDown k        -> keyboard %~ Set.insert k
  KeyUp k          -> keyboard %~ Set.delete k
  MouseDown b      -> mouse.buttons %~ Set.insert k
  MouseUp b        -> mouse.buttons %~ Set.delete k
  MouseScroll δ    -> scroll %~ (+ δ)
  WindowClosing    -> id
  _                -> id

-- | 
run :: EitherT String IO ()
run = do
  GLFW.init
