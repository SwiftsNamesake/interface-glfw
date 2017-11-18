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
--        - Start blog about programming and library work
--        - Vulkan (?)
--        - Refactor
--        - Distribution, packing, patching
--        - Rendering system, compositing, multithreading, resizing textures, framebuffers
--        - Layout engine (eg. grid layout)

-- This package is intended to form the basis of an ecosystem of native Haskell
-- UI components.
--
-- I've yet to settle on a model (FRP, MVC, or just reusable pieces of logic).
-- Perhaps it's appropriate if we simply lay the foundation for UI rendering
-- and allow other parties to build abstractions on top of it.

-- GHC Directives --------------------------------------------------------------

{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}

-- API -------------------------------------------------------------------------

module Graphics.UIKit where
  
-- We'll need these ------------------------------------------------------------

import           Data.AABB as AABB

import           Data.Monoid ((<>))
import           Data.Maybe
import qualified Data.Vector.Storable as VS
import           Data.Vector.Storable (Storable(..))
import           Data.Foldable
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.ByteString as B

import Control.Monad.Loops (whileJust)
import Control.Monad.Trans.Either
import Control.Concurrent.STM
import Control.Concurrent.Async hiding (link)
import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad
import Control.Exception.Safe

import Linear
import Lens.Micro.Platform

import System.Random as Random

import qualified Graphics.UI.GLFW            as GLFW
import           Graphics.UI.GLFW            (MouseButton(..), Key(..), KeyState(..), MouseButtonState(..), CursorState(..))
import qualified Graphics.Rendering.OpenGL   as GL
import           Graphics.Rendering.OpenGL   (($=))
import           Graphics.Rasterific         as Rasterific hiding (Vector)
import           Graphics.Rasterific.Texture as Rasterific
import qualified Graphics.Text.TrueType      as Font
import           Graphics.Text.TrueType      (Font(..), Dpi(..), BoundingBox(..))

import Codec.Picture (Image(..), PixelRGB8(..), PixelRGBA8(..), DynamicImage(..), savePngImage)

import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (plusPtr, nullPtr, Ptr)
import Foreign.Storable (sizeOf)

-- * 
import qualified Geometry.Sculptor.Shapes as Geometry

-- *
import Control.Loops

-- *
import Graphics.UIKit.Types
import Graphics.UIKit.Lenses

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
frameBounds :: GLFW.Window -> IO (V2 Int)
frameBounds win = vectorise (GLFW.getFramebufferSize win)

--------------------------------------------------------------------------------

-- |
setup :: String -> V2 Int -> EitherT String IO (GLFW.Window, MessageChannel)
setup title (V2 dx dy) = do
  insist "Failed to initialise GLFW" =<< lift GLFW.init
  win <- explain "Failed to create window" =<< lift (GLFW.createWindow dx dy title Nothing Nothing)
  lift . GLFW.makeContextCurrent $ Just win
  ch <- lift $ setupEvents win
  return (win, ch)


-- |
setupEvents :: GLFW.Window -> IO MessageChannel
setupEvents win = do
  channel <- newTChanIO
  let put = atomically . writeTChan channel -- TODO | - Return this (?)
  
  GLFW.setCursorPosCallback       win (Just $ \win mx my      -> put . MouseMotion $ fmap realToFrac (V2 mx my))
  GLFW.setCursorEnterCallback     win (Just $ \win st         -> put $ makeFocus st)
  GLFW.setMouseButtonCallback     win (Just $ \win b st mod   -> put $ makeMouse b st mod)
  GLFW.setKeyCallback             win (Just $ \win k r st mod -> put $ makeKey k r st mod)
  GLFW.setDropCallback            win (Just $ \win fns        -> put $ FileDrop fns)
  GLFW.setFramebufferSizeCallback win (Just $ \win dx dy      -> put . FrameResize  $ V2 dx dy)
  GLFW.setWindowSizeCallback      win (Just $ \win dx dy      -> put . WindowResize $ V2 dx dy)
  GLFW.setWindowPosCallback       win (Just $ \win px py      -> put . WindowMove   $ V2 px py)
  --GLFW.setErrorCallback $ Just (\err s -> _)
  -- setClipboardString, getClipboardString
  return channel
  where
    makeFocus CursorState'InWindow = MouseEnter
    makeFocus _                    = MouseLeave

    makeMouse b MouseButtonState'Pressed  _ = MouseDown b
    makeMouse b MouseButtonState'Released _ = MouseUp b

    makeKey k _ (KeyState'Pressed)   mods = KeyDown k
    makeKey k _ (KeyState'Released)  mods = KeyUp k
    makeKey k _ (KeyState'Repeating) mods = KeyRepeat k


-- |
-- TODO | - Rename (?)
initial :: GLFW.Window -> IO Input
initial win = Input
                <$> pure (V2 0 0)
                <*> windowBounds win
                <*> frameBounds win
                <*> (Mouse <$> vectorise (GLFW.getCursorPos win) <*> pure Set.empty)
                <*> pure Set.empty
                <*> (GLFW.getTime >>= \(Just t) -> pure t)

-- |
-- TODO | - How do we deal with repeat events (does it even matter)?
--        - We'll probably want to replace this logic when we move on to proper FRP
-- processMessages :: Input -> (Input -> s -> SystemEvent -> IO s) -> s -> IO s
processMessages :: MessageChannel -> (SystemEvent -> app -> IO app) -> app -> IO app
processMessages channel dispatch app = do
  messages <- queuedMessages channel
  foldrM dispatch app messages


-- |
queuedMessages :: MessageChannel -> IO [SystemEvent]
queuedMessages channel = atomically $ whileJust (tryReadTChan channel) return


-- | Simple way of posting Animation events
animate :: (SystemEvent -> IO ()) -> Int -> IO (Async ())
animate post fps = async . forever $ do
  post Animate
  threadDelay $ div (10^6) fps

-- Events ----------------------------------------------------------------------

-- | Updates the 'Input', given a 'SystemEvent'
-- TODO | - Rename to reflect exactly what this function does (?)
onevent :: SystemEvent -> Input -> Input
onevent e = case e of
  MouseMotion pos  -> mouse.cursor .~ pos
  KeyDown k        -> keyboard %~ Set.insert k
  KeyUp k          -> keyboard %~ Set.delete k
  MouseDown b      -> mouse.buttons %~ Set.insert b
  MouseUp b        -> mouse.buttons %~ Set.delete b
  MouseScroll δ    -> scroll %~ (+ δ)
  FrameResize   p  -> frameSize       .~ p -- TODO | - Make sure this is correct
  WindowResize  p  -> windowRect.size .~ p -- TODO | - Make sure this is correct
  WindowMove    p  -> windowRect.lo   .~ p -- TODO | - Make sure this is correct
  WindowClosing    -> id -- TODO | - Should 'Input' record the this (?)
  _                -> id

---------------------

-- | Temporary scene type
data Scene = Scene {
  window  :: GLFW.Window,
  mesh    :: VAODescriptor,
  texture :: GL.TextureObject,
  program :: GL.Program,
  font    :: Font,
  --input   :: Input,
  channel :: MessageChannel
}

-----------------------------------------

-- | 
runApplication :: String
               -> V2 Int
               -> (Scene -> app -> Image PixelRGBA8)
               -> (SystemEvent -> app -> app)
               -> (Input -> EitherT String IO app)
               -> IO (Either String ())
runApplication wtitle wsize draw update makeApp = runEitherT $ do
  (win, channel) <- setup wtitle wsize
  
  program <- newShader "assets/shaders/textured.vert" "assets/shaders/textured.frag"
  lift (GL.currentProgram $= Just program)

  quad <- lift $ newQuad (V2 2 2)
  font <- EitherT $ Font.loadFontFile "assets/fonts/3Dumb.ttf"

  -- Texture
  -- TODO | - Don't hard-code the texture
  tex <- lift newTexture
  lift $ setTexture tex

  input <- lift $ initial win
  
  let scene = Scene win quad tex program font channel
  
  app <- makeApp input

  lift . uploadTexture $ draw scene app
  lift $ loop scene draw update app


-- |
loop :: Scene -> (Scene -> app -> Image PixelRGBA8) -> (SystemEvent -> app -> app) -> app -> IO ()
loop scene draw update app = do
  new <- processMessages (scene~>to channel) (\msg old -> return $ update msg old) app
  render scene $ draw scene new
  GLFW.pollEvents
  close <- GLFW.windowShouldClose $ scene~>to window
  unless close $ loop scene draw update new

--------------------------------------------------------------------------------

-- Render ----------------------------------------------------------------------

-- | Creates a 'TextureObject', binds and enables it, and sets the filtering and wrapping
--   properties to reasonble defaults.
newTexture :: IO GL.TextureObject
newTexture = do
  tex <- GL.genObjectName
  setTexture tex
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  return tex


-- | Enables and binds a 'TextureObject' to a unit.
setTexture :: GL.TextureObject -> IO ()
setTexture tex = do
  GL.texture GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just tex


-- |
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


-- | Creates and binds a 'VertexArrayObject'
newVAO :: IO GL.VertexArrayObject
newVAO = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  return vao


-- |
-- TODO | - Find structured way of doing this (eg. type class, type family)
--        - Consider EitherT
newAttribute :: (Storable (v a), Foldable v) => GL.AttribLocation -> [v a] -> IO AttributeDescriptor
newAttribute location vs = do
  buffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just buffer
  withArray vs $ \ptr -> GL.bufferData GL.ArrayBuffer $= (bufferSize, ptr, GL.StaticDraw)

  GL.vertexAttribPointer location $= (GL.ToFloat, GL.VertexArrayDescriptor nComponents GL.Float 0 (bufferOffset firstIndex))
  GL.vertexAttribArray location $= GL.Enabled
  return $ AttributeDescriptor buffer bufferSize (fromIntegral nComponents)
  where
    firstIndex  = 0
    bufferSize  = fromIntegral $ numVertices * maybe 0 sizeOf (listToMaybe vs) -- TODO | - Improve
    nComponents = maybe 3 (fromIntegral . length) (listToMaybe vs) -- TODO | - Improve (don't hard-code default)
    numVertices = length vs


-- |
newQuad :: V2 GL.GLfloat -> IO VAODescriptor
newQuad (V2 dx dy) = do
  vao     <- newVAO
  vBuffer <- newAttribute (GL.AttribLocation 0) vs -- TODO | - Do not hard-code location
  tBuffer <- newAttribute (GL.AttribLocation 2) uv -- TODO | - Do not hard-code location
  return $ VAODescriptor vao 0 (fromIntegral $ count (vBuffer :: AttributeDescriptor))
  where
    -- TODO | - Should probably add uv coords to cubist-sculptor
    uv = fmap (\v -> pure 0.5 + fmap ((/2) . signum) (v~>_xy)) vs :: [V2 GL.GLfloat] -- Texture coordinates
    vs = concat . Geometry.triangles $ Geometry.planeXY (\x y z -> V4 x y z 1) dx dy :: [V4 GL.GLfloat]

--------------------------------------------------------------------------------

-- | A sketch of how the rendering might work.
render :: Scene -> Image PixelRGBA8 -> IO ()
render scene im = do
  --let (V2 dx dy) = scene~>to input.frameSize
  -- TODO | - Figoure out of to deal with 'Input'
  (V2 dx dy) <- frameBounds (scene~>to window)
  GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral dx) (fromIntegral dy))
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer]

  setTexture $ scene~>to texture
  GL.uniform (GL.UniformLocation 0) $= (GL.TextureUnit 0) -- TODO | - Don't hard-code uniform location (IMPORTANT!)
  refreshTexture $ im

  renderVAO $ scene~>to mesh
  GLFW.swapBuffers $ scene~>to window


-- |
renderVAO :: VAODescriptor -> IO ()
renderVAO (VAODescriptor triangles firstIndex numVertices) = do
  GL.bindVertexArrayObject $= Just triangles
  GL.drawArrays GL.Triangles firstIndex numVertices

--------------------------------------------------------------------------------

-- TODO | - Should we care about GL.deleteObjectName

-- |
newShader :: FilePath -> FilePath -> EitherT String IO GL.Program
newShader vPath fPath = handleIO
                          (left . show)                           
                          (do program <- lift $ GL.createProgram
                              vertex   <- shaderComponent program vPath GL.VertexShader
                              fragment <- shaderComponent program fPath GL.FragmentShader
                              link program)


-- |
shaderComponent :: GL.Program -> FilePath -> GL.ShaderType -> EitherT String IO GL.Shader
shaderComponent program path kind = do
  (ok, shader) <- lift $ do
    shader <- GL.createShader kind
    src <- B.readFile path
    GL.shaderSourceBS shader $= src
    GL.compileShader shader
    ok <- GL.get $ GL.compileStatus shader
    return (ok, shader)
  unless ok $ lift (GL.get $ GL.shaderInfoLog shader) >>= left
  lift $ GL.attachShader program shader
  right shader


-- |
link :: GL.Program -> EitherT String IO GL.Program
link program = do
  lift $ GL.linkProgram program
  ok <- lift $ GL.get (GL.linkStatus program)
  unless ok $ do
    log <- lift $ GL.get (GL.programInfoLog program)
    left log
  right program


-- |
--ensure :: _
--ensure = do

--------------------------------------------------------------------------------

-- | Upload pixel data to texture object, for the first time (cf. also texSubImage2d)
--   Make sure the right texture is bound at this point.
-- TODO | - Rename (?)
uploadTexture :: Image PixelRGBA8 -> IO ()
uploadTexture (Image width height dat) = do
  -- Access the data vector pointer
  VS.unsafeWith dat $ \ptr ->
    GL.texImage2D    -- Generate the texture
      (GL.Texture2D) --
      (GL.NoProxy)   -- No proxy
      (0)            -- No mipmaps
      (GL.RGBA8)     -- Internal storage format: use R8G8B8A8 as internal storage
      (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) -- Size of the image
      (0)                                        -- No borders
      (GL.PixelData GL.RGBA GL.UnsignedByte ptr) -- The pixel data: the vector contains Bytes, in RGBA order


-- |
refreshTexture :: Image PixelRGBA8 -> IO ()
refreshTexture (Image width height dat) = do
  VS.unsafeWith dat $ \ptr ->
    GL.texSubImage2D
      (GL.Texture2D)
      (0)
      (GL.TexturePosition2D 0 0)
      (GL.TextureSize2D (fromIntegral width) (fromIntegral height))
      (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

--------------------------------------------------------------------------------

-- | Set texture coordinate wrapping options for both the 'S' and 'T'
--   dimensions of a 2D texture.
-- Borrowed from GLUtil
texture2DWrap :: GL.StateVar (GL.Repetition, GL.Clamping)
texture2DWrap = GL.makeStateVar
                  (GL.get (GL.textureWrapMode GL.Texture2D GL.S))
                  (forM_ [GL.S,GL.T] . aux)
  where aux x d = GL.textureWrapMode GL.Texture2D d $= x

--------------------------------------------------------------------------------

-- | 
aabb :: AABB V2 Float -> [Primitive]
aabb bounds = rectangle (bounds~>lo) (bounds~>size.x) (bounds~>size.y)

--------------------------------------------------------------------------------

-- * Borrowed from Pixels

-- TODO | - Factor out

-- |
-- TODO | - Sort out the terminology, rename parameters
--        - Factor out
--        - Baseline height (?)
--        - Retrun AABB instead (?)
anchoredTo :: Font -> Dpi -> PointSize -> String -> V2 Float -> V2 Float -> V2 Float
anchoredTo font dpi pt s anchor p = let box = stringBounds font dpi pt s in p - anchor * (box~>size) {- SW to NW, TODO | - More general solution -}-- + V2 0 (box~>size.y)

-- |
--anchoredTo :: Font -> Dpi -> PointSize -> String -> V2 Float -> V2 Float -> V2 Float

-- |
anchoredText :: Font -> Dpi -> PointSize -> String -> V2 Float -> V2 Float -> Drawing px ()
anchoredText font dpi pt s anchor p = let lo = anchoredTo font dpi pt s anchor p in printTextAt font pt lo s

-- |
toAABB :: BoundingBox -> AABB V2 Float
toAABB box@(BoundingBox x₀ y₀ x₁ y₁ _) = AABB (V2 x₀ y₀) (V2 x₁ y₁)

-- |
stringBounds :: Font -> Dpi -> PointSize -> String -> AABB V2 Float
stringBounds font dpi pt = toAABB . Font.stringBoundingBox font dpi pt
