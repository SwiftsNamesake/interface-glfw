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

-- This package is intended to form the basis of an ecosystem of native Haskell
-- UI components.
--
-- I've yet to settle on a model (FRP, MVC, or just reusable pieces of logic).
-- Perhaps it's appropriate if we simply lay the foundation for UI rendering
-- and allow other parties to build abstractions on top of it.

-- GHC Directives --------------------------------------------------------------

{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- API -------------------------------------------------------------------------

module Graphics.UIKit where
  
-- We'll need these ------------------------------------------------------------

import           Data.AABB as AABB

import           Data.Monoid ((<>))
import           Data.Maybe
import qualified Data.Vector.Storable as V
import           Data.Foldable
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Text as Text
import qualified Data.ByteString as B

import Control.Monad.Loops (whileJust)
import Control.Monad.Trans.Either
import Control.Concurrent.STM
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad
import Control.Exception.Safe

import Linear
import Lens.Micro

import qualified Graphics.UI.GLFW as GLFW
import           Graphics.UI.GLFW (MouseButton(..), Key(..), KeyState(..), MouseButtonState(..), CursorState(..))
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))
import           Graphics.Rasterific as Rasterific
import           Graphics.Rasterific.Texture as Rasterific

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
  let put = atomically . writeTChan channel
  
  GLFW.setCursorPosCallback   win (Just $ \win mx my      -> put $ MouseMotion $ fmap realToFrac (V2 mx my))
  GLFW.setCursorEnterCallback win (Just $ \win st         -> put $ makeFocus st)
  GLFW.setMouseButtonCallback win (Just $ \win b st mod   -> put $ makeMouse b st mod)
  GLFW.setKeyCallback         win (Just $ \win k r st mod -> put $ makeKey k r st mod)
  GLFW.setDropCallback        win (Just $ \win fns        -> put $ FileDrop fns)
  -- GLFW.setFramebufferSizeCallback win (Just $ \win dx dy -> put $ V2 _ _)
  -- GLFW.setWindowSizeCallback  win (Just $ \win dx dy -> put $ V2 _ _)
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
                <*> pure (Set.empty)
                <*> (GLFW.getTime >>= \(Just t) -> pure t)


-- |
-- TODO | - How do we deal with repeat events (does it even matter)?
--        - We'll probably want to replace this logic when we move on to proper FRP
-- processMessages :: Input -> (Input -> s -> SystemEvent -> IO s) -> s -> IO s
processMessages :: MessageChannel -> (app -> SystemEvent -> IO app) -> app -> IO app
processMessages channel dispatch app = do
  messages <- queuedMessages channel
  foldrM (\msg app -> dispatch app msg) app messages


-- |
queuedMessages :: MessageChannel -> IO [SystemEvent]
queuedMessages channel = atomically $ whileJust (tryReadTChan channel) return

-- Events ----------------------------------------------------------------------

-- | Updates the 'Input', given a 'SystemEvent'
-- TODO | - 
onevent :: SystemEvent -> Input -> Input
onevent e = case e of
  MouseMotion pos  -> mouse.cursor .~ pos
  KeyDown k        -> keyboard %~ Set.insert k
  KeyUp k          -> keyboard %~ Set.delete k
  MouseDown b      -> mouse.buttons %~ Set.insert b
  MouseUp b        -> mouse.buttons %~ Set.delete b
  MouseScroll δ    -> scroll %~ (+ δ)
  WindowClosing    -> id -- TODO | - Should 'Input' record the this (?)
  _                -> id


-- | 
run :: IO (Either String ()) -- EitherT String IO ()
run = runEitherT $ do
  (win, channel) <- setup "UIKit" (V2 600 400)
  program <- newShader' "assets/shaders/textured.vert" "assets/shaders/textured.frag"
  lift (GL.currentProgram $= Just program)
  quad <- lift newQuad

  -- Texture
  -- TODO | - Don't hard-code the texture
  tex <- lift newTexture
  lift $ setTexture tex
  lift . uploadTexture $ drawing (V2 600 400)

  lift $ loop (win, quad, tex, program, channel)


-- |
loop :: (GLFW.Window, VAODescriptor, GL.TextureObject, GL.Program, MessageChannel) -> IO ()
loop app@(win, quad, tex, program, channel) = do
  new <- processMessages channel (\old msg -> return old) app
  render (win, V2 600 400, tex, program, quad)
  GLFW.pollEvents
  close <- GLFW.windowShouldClose win
  unless close (loop new)


-- Render ----------------------------------------------------------------------

-- |
newTexture :: IO GL.TextureObject
newTexture = do
  tex <- GL.genObjectName
  setTexture tex
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  texture2DWrap $= (GL.Repeated, GL.ClampToEdge)
  return tex


-- |
setTexture :: GL.TextureObject -> IO ()
setTexture tex = do
  GL.texture GL.Texture2D $= GL.Enabled
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just tex


-- |
bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral


-- |
newVAO :: IO GL.VertexArrayObject
newVAO = do
  vao <- GL.genObjectName
  GL.bindVertexArrayObject $= Just vao
  return vao

-- |
-- TODO | - Find structured way of doing this (eg. type class, type family)
--        - Consider EitherT
newAttribute :: (V.Storable (v a), Foldable v) => GL.AttribLocation -> [v a] -> IO AttributeDescriptor
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
newShader :: IO GL.Program
newShader = loadShaders [
  ShaderInfo GL.VertexShader (FileSource "assets/shaders/textured.vert"),
  ShaderInfo GL.FragmentShader (FileSource "assets/shaders/textured.frag")]


-- |
newQuad :: IO VAODescriptor
newQuad = do
  vao     <- newVAO
  vBuffer <- newAttribute (GL.AttribLocation 0) vs -- TODO | - Do not hard-code location
  tBuffer <- newAttribute (GL.AttribLocation 2) uv -- TODO | - Do not hard-code location
  return $ VAODescriptor vao 0 (fromIntegral $ count (vBuffer :: AttributeDescriptor))
  where
    -- TODO | - Should probably add uv coords to cubist-sculptor
    uv = fmap (\(V4 x y _ _) -> pure 0.5 + V2 x y) vs :: [V2 GL.GLfloat] -- Texture coordinates
    vs          = concat . Geometry.triangles $ Geometry.planeXY (\x y z -> V4 x y z 1) 2 2 :: [V4 GL.GLfloat]

--------------------------------------------------------------------------------

-- | A sketch of how the rendering might work.
render :: (GLFW.Window, V2 Int, GL.TextureObject, GL.Program, VAODescriptor) -> IO ()
render (win, V2 cx cy, tex, program, VAODescriptor triangles firstIndex numVertices) = do
  GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral cx) (fromIntegral cy))
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer]

  setTexture tex
  --loc <- GL.get (GL.uniformLocation program "tex")
  GL.uniform (GL.UniformLocation 0) $= (GL.TextureUnit 0) -- TODO | - Don't hard-code uniform location

  GL.bindVertexArrayObject $= Just triangles
  GL.drawArrays GL.Triangles firstIndex numVertices
  GLFW.swapBuffers win

--------------------------------------------------------------------------------

data ShaderSource =
     ByteStringSource B.ByteString
     -- ^ The shader source code is directly given as a 'B.ByteString'.
   | StringSource String
     -- ^ The shader source code is directly given as a 'String'.
   | FileSource FilePath
     -- ^ The shader source code is located in the file at the given 'FilePath'.
   deriving ( Eq, Ord, Show )

getSource :: ShaderSource -> IO B.ByteString
getSource (ByteStringSource bs) = return bs
getSource (StringSource str) = return $ GL.packUtf8 str
getSource (FileSource path) = B.readFile path

data ShaderInfo = ShaderInfo GL.ShaderType ShaderSource deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- TODO | - Should we care about GL.deleteObjectName

-- |
newShader' :: FilePath -> FilePath -> EitherT String IO GL.Program
newShader' vPath fPath = handleIO
                           (left . show)                           
                           (do program <- lift $ GL.createProgram
                               vertex   <- shaderComponent program vPath GL.VertexShader
                               fragment <- shaderComponent program fPath GL.FragmentShader
                               link program)
  --where
    --tryCreateResource create delete f = bracketOnError create delete f


-- |
shaderComponent :: GL.Program -> FilePath -> GL.ShaderType -> EitherT String IO GL.Shader
shaderComponent program path kind = do
  shader <- lift (GL.createShader kind)
  src <- lift (B.readFile path)
  lift $ GL.shaderSourceBS shader $= src
  lift $ GL.compileShader shader
  ok <- lift $ GL.get (GL.compileStatus shader)
  unless ok $ do
    log <- lift $ GL.get (GL.shaderInfoLog shader)
    left log
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
--ensure check log = do

--------------------------------------------------------------------------------

-- | Upload pixel data to texture object, for the first time (cf. also texSubImage2d)
--   Make sure the right texture is bound at this point.
-- TODO | - Rename (?)
uploadTexture :: Image PixelRGBA8 -> IO ()
uploadTexture (Image width height dat) = do
  -- Access the data vector pointer
  V.unsafeWith dat $ \ptr ->
    GL.texImage2D -- Generate the texture
      GL.Texture2D
      GL.NoProxy  -- No proxy
      0           -- No mipmaps
      GL.RGBA8    -- Internal storage format: use R8G8B8A8 as internal storage
      (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) -- Size of the image
      0                               -- No borders
      (GL.PixelData GL.RGBA GL.UnsignedByte ptr) -- The pixel data: the vector contains Bytes, in RGBA order


-- |
refreshTexture :: Image PixelRGBA8 -> IO ()
refreshTexture (Image width height dat) = do
  V.unsafeWith dat $ \ptr ->
    GL.texSubImage2D
      (GL.Texture2D)
      (0)
      (GL.TexturePosition2D 0 0)
      (GL.TextureSize2D (fromIntegral width) (fromIntegral height))
      (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

--------------------------------------------------------------------------------

-- | Set texture coordinate wrapping options for both the 'S' and 'T'
-- dimensions of a 2D texture.
texture2DWrap :: GL.StateVar (GL.Repetition, GL.Clamping)
texture2DWrap = GL.makeStateVar (GL.get (GL.textureWrapMode GL.Texture2D GL.S))
                             (forM_ [GL.S,GL.T] . aux)
  where aux x d = GL.textureWrapMode GL.Texture2D d $= x

--------------------------------------------------------------------------------

-- |
drawing :: V2 Int -> Image PixelRGBA8
drawing (V2 dx dy) = renderDrawing dx dy white $ do
                       withTexture (uniformTexture $ PixelRGBA8 0 0 0 255) $
                         stroke 3 JoinRound (CapStraight 0, CapStraight 0) path
                       withTexture (uniformTexture drawColor) $ do
                         fill $ circle (V2 300 200) 140
                         withPathOrientation path 0 $ do
                             --printTextAt font (PointSize 24) (V2 0 0) "Text on path"
                             forM_ [10, 20 .. 100] $ \r -> fill $ circle (V2 0 0) 10
                             --fill $ rectangle (V2 0 0) 30 20
                             --fill $ rectangle (V2 0 0) 10 20
                             --fill $ rectangle (V2 0 0) 20 20
                             --fill $ rectangle (V2 0 0) 20 50    
  where
    white = PixelRGBA8 255 255 255 255
    drawColor = PixelRGBA8 0 0x86 0xc1 255
    path = Path (V2 100 180) False [PathCubicBezierCurveTo (V2 20 20) (V2 170 20) (V2 300 200)]

