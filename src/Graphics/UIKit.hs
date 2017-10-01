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

import           Data.AABB
import           Data.Foldable
import qualified Data.Set
import           Data.Set (Set)

import Control.Monad.Trans.Either
import Control.Concurrent.STM

import Linear
import Lens.Micro

import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW (MouseButton(..), Key(..), KeyState(..), MouseButtonState(..))

-- Definitions -----------------------------------------------------------------

-- |
-- TODO | - Timestamps
--        -
-- newtype InputChannel = InputChannel (TChan SystemEvent)
type InputChannel = TChan SystemEvent

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
  fWindowRect   :: AABB V2 Int,
  fMouse        :: Mouse,
  fKeyboard     :: Set Key,
  fTime         :: Float, -- Current time (updated on each `tick`)
  fInputChannel :: InputChannel
} -- deriving (Show)

-- |
data Mouse = Mouse {
  fCursor  :: V2 Float,
  fButtons :: Set MouseButton
} deriving (Show)

--------------------------------------------------------------------------------

-- TODO | - Factor out

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
setup :: V2 Int -> EitherT String IO App
setup (V2 dx dy) = do
  insist "Failed to initialise GLFW" =<< lift GLFW.init
  win <- explain "Failed to create window" =<< lift (GLFW.createWindow dx dy "Visual Overlap" Nothing Nothing)
  lift . GLFW.makeContextCurrent $ Just win
  es <- lift $ setupEvents win
  return $ App { fInput = es, fWindow = win }


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
  -- TODO | - Factor out so we can reuse (better yet, use callbacks so we can react to resize events)
  cur <- uncurry V2 <$> GLFW.getCursorPos win
  pos <- uncurry V2 <$> GLFW.getWindowPos win  -- TODO: Client (eg. frame buffer) and full window (this is probably the window itselfs)
  sz  <- uncurry V2 <$> GLFW.getWindowSize win -- TODO: Same here (cf. frame buffer size)
  return $ Input {
    fMessages   = chan,
    fTime       = t,
    fWindowRect = AABB (pos) (pos+sz),
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
processEvents :: Input ap -> IO (Input app)
processEvents bundle = do
  events <- liftIO $ whileJust (atomically . tryReadTChan $ bundle^.inputChannel) return
  sz     <- GLFW.getWindowSize (bundle~>window) -- GLFW.getFrameBufferSize (app^.window)
  Just t <- GLFW.getTime
  let app' = app & (input.size .~ sz)
                 & (input.time .~ realToFrac t)
                 & (viewport   .~ (V2 0 0) sz)
             in foldrM onevent app' events

-- Events --------------------------------------------------------------------------------------------------------------------------------------------

-- | Updates the 'Input', given a 'SystemEvent'
-- TODO | - 
onevent :: SystemEvent -> Input app -> Input app
onevent e = case e of
  MouseMotion pos' -> mouse.cursor .~ pos
  KeyDown k        -> keyboard.contains k .~ True
  KeyUp k          -> keyboard.contains k .~ False
  MouseDown b      -> mouse.buttons.contains b .~ True
  MouseUp b        -> mouse.buttons.contains b .~ False
  MouseScroll δ    -> scroll %~ (+ δ)) δ
  WindowClosing    -> id
  _                -> id
  where
    contains :: (Ord a) => a -> Lens' (Set a) Bool
    contains k f s = let new True  = Set.insert k s
                         new False = Set.delete k s
                     in new <$> f (Set.member k s)

-- | 
run :: EitherT String IO ()
run = do
  GLFW.init
