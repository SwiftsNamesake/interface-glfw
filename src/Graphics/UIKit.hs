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
import Control.Exception

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

import qualified Geometry.Sculptor.Shapes as Geometry

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

--------------------------------------------------------------------------------

-- TODO | - Factor out

-- | A poor man's over
(~>) :: s -> SimpleGetter s a -> a
(~>) = (^.)

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

--------------------------------------------------------------------------------

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
  (quad, tex, program) <- lift newQuad
  lift $ loop (win, quad, tex, program, channel)
  where
    loop :: (GLFW.Window, Descriptor, GL.TextureObject, GL.Program, MessageChannel) -> IO ()
    loop app@(win, quad, tex, program, channel) = do
      new <- processMessages channel (\old msg -> return old) app
      render (win, V2 600 400, tex, program, quad)
      GLFW.pollEvents
      close <- GLFW.windowShouldClose win
      unless close (loop new)


-- Render ----------------------------------------------------------------------

-- |
data Descriptor = Descriptor GL.VertexArrayObject GL.ArrayIndex GL.NumArrayIndices deriving (Eq, Show)


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
--newVAO :: IO Descriptor

{-
newBuffer :: IO _
newBuffer _ = do
  buffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just buffer
  withArray vs $ \ptr -> GL.bufferData GL.ArrayBuffer $= (vBufferSize, ptr, GL.StaticDraw)

  let vFirstIndex = 0
      vPosition  = GL.AttribLocation 0
  GL.vertexAttribPointer vPosition $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 0 (bufferOffset vFirstIndex))
  GL.vertexAttribArray vPosition $= GL.Enabled
  return buffer
-}


-- |
newQuad :: IO (Descriptor, GL.TextureObject, GL.Program)
newQuad = do
  
  -- Assemble the shader program
  program <- loadShaders [
    ShaderInfo GL.VertexShader (FileSource "assets/shaders/textured.vert"),
    ShaderInfo GL.FragmentShader (FileSource "assets/shaders/textured.frag")]
  GL.currentProgram $= Just program

  -- Create VAO
  triangles <- GL.genObjectName
  GL.bindVertexArrayObject $= Just triangles

  -- Vertices
  vertexBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
  withArray vs $ \ptr -> GL.bufferData GL.ArrayBuffer $= (vBufferSize, ptr, GL.StaticDraw)

  let vFirstIndex = 0
      vPosition  = GL.AttribLocation 0
  GL.vertexAttribPointer vPosition $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 0 (bufferOffset vFirstIndex))
  GL.vertexAttribArray vPosition $= GL.Enabled

  -- Texture coordinates
  textureBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just textureBuffer
  withArray uv $ \ptr -> GL.bufferData GL.ArrayBuffer $= (tBufferSize, ptr, GL.StaticDraw)
  let tFirstIndex = 0
      uvCoords = GL.AttribLocation 2
  GL.vertexAttribPointer uvCoords $= (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 (bufferOffset tFirstIndex))
  GL.vertexAttribArray uvCoords $= GL.Enabled
  
  -- Colour
  textureBuffer <- GL.genObjectName
  GL.bindBuffer GL.ArrayBuffer $= Just textureBuffer
  withArray co $ \ptr -> GL.bufferData GL.ArrayBuffer $= (cBufferSize, ptr, GL.StaticDraw)
  let cFirstIndex = 0
      vColour = GL.AttribLocation 1
  GL.vertexAttribPointer vColour $= (GL.ToFloat, GL.VertexArrayDescriptor 4 GL.Float 0 (bufferOffset cFirstIndex))
  GL.vertexAttribArray vColour $= GL.Enabled

  -- Texture
  tex <- newTexture
  putStrLn $ "tex object: " <> show tex
  setTexture tex
  loc <- GL.get (GL.uniformLocation program "tex")
  GL.uniform loc $= (GL.TextureUnit 0)
  putStrLn $ "Tex uniform: " <> show loc
  --savePngImage "assets/out.png" (ImageRGBA8 drawing)
  uploadTexture drawing

  --GettableStateVar [(GLint, VariableType, String)]
  mapM_ print =<< (GL.get $ GL.activeUniforms program)

  -- Phew
  return $ (Descriptor triangles vFirstIndex (fromIntegral numVertices), tex, program)
  where
    vs          = concat . Geometry.triangles $ Geometry.planeXY (\x y z -> V4 x y z 1) 2 2 :: [V4 GL.GLfloat]
    vBufferSize = fromIntegral $ numVertices * maybe 0 sizeOf (listToMaybe vs)
    numVertices = length vs

    co = concat . Geometry.triangles . zipWith const (cycle [V4 1 0 0 1, V4 0 1 0 1, V4 0 0 1 1]) $ Geometry.planeXY (\_ _ _ -> V4 0 0 0 0) 1 1 :: [V4 GL.GLfloat]
    cBufferSize = fromIntegral $ numVertices * maybe 0 sizeOf (listToMaybe co)

    -- TODO | - Should probably add uv coords to cubist-sculptor
    uv = fmap (\(V4 x y _ _) -> pure 0.5 + V2 x y) vs :: [V2 GL.GLfloat] -- Texture coordinates
    tBufferSize = fromIntegral $ numVertices * maybe 0 sizeOf (listToMaybe uv)

---

crashOnError = GL.get GL.errors >>= \es -> unless (null es) (print es >> error "OpenGL errors halted the process")

-- | A sketch of how the rendering might work.
render :: (GLFW.Window, V2 Int, GL.TextureObject, GL.Program, Descriptor) -> IO ()
render (win, V2 cx cy, tex, program, Descriptor triangles firstIndex numVertices) = do
  crashOnError
  GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral cx) (fromIntegral cy))
  GL.clearColor $= GL.Color4 0 0 0 1
  GL.clear [GL.ColorBuffer]
  -- TODO | - Set uniforms
  
  --with (distribute $ triangleTransformation t) $ \ptr ->
  --  GL.glUniformMatrix4fv loc 1 0 (castPtr (ptr :: Ptr (M44 CFloat)))
  crashOnError
  setTexture tex
  loc <- GL.get (GL.uniformLocation program "tex")
  GL.uniform loc $= (GL.TextureUnit 0)
  crashOnError
  GL.bindVertexArrayObject $= Just triangles
  crashOnError
  GL.drawArrays GL.Triangles firstIndex numVertices
  crashOnError
  GLFW.swapBuffers win
  crashOnError

------

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

------

loadShaders :: [ShaderInfo] -> IO GL.Program
loadShaders infos = GL.createProgram `bracketOnError` GL.deleteObjectName $ \program -> do
  loadCompileAttach program infos
  linkAndCheck program
  return program

linkAndCheck :: GL.Program -> IO ()
linkAndCheck = checked GL.linkProgram GL.linkStatus GL.programInfoLog "link"

loadCompileAttach :: GL.Program -> [ShaderInfo] -> IO ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) =
  GL.createShader shType `bracketOnError` GL.deleteObjectName $ \shader -> do
      src <- getSource source
      GL.shaderSourceBS shader $= src
      compileAndCheck shader
      GL.attachShader program shader
      loadCompileAttach program infos

compileAndCheck :: GL.Shader -> IO ()
compileAndCheck = checked GL.compileShader GL.compileStatus GL.shaderInfoLog "compile"

checked :: (t -> IO ())
        -> (t -> GL.GettableStateVar Bool)
        -> (t -> GL.GettableStateVar String)
        -> String
        -> t
        -> IO ()
checked action getStatus getInfoLog message object = do
  action object
  ok <- GL.get (getStatus object)
  unless ok $ do
    infoLog <- GL.get (getInfoLog object)
    fail (message <> " log: " <> infoLog)

--------------

-- | Upload pixel data to texture object
--   Borrowed from SO
-- TODO | - Rename (?)
-- GL.textureBinding GL.Texture2D $= Just tex
uploadTexture :: Image PixelRGBA8 -> IO ()
uploadTexture (Image width height dat) = do
  -- Access the data vector pointer
  crashOnError
  V.unsafeWith dat $ \ptr ->
    GL.texImage2D -- Generate the texture
      GL.Texture2D
      GL.NoProxy  -- No proxy
      0           -- No mipmaps
      GL.RGBA8    -- Internal storage format: use R8G8B8A8 as internal storage
      (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) -- Size of the image
      0                               -- No borders
      (GL.PixelData GL.RGBA GL.UnsignedByte ptr) -- The pixel data: the vector contains Bytes, in RGBA order
  crashOnError

--------------------

-- | Set texture coordinate wrapping options for both the 'S' and 'T'
-- dimensions of a 2D texture.
texture2DWrap :: GL.StateVar (GL.Repetition, GL.Clamping)
texture2DWrap = GL.makeStateVar (GL.get (GL.textureWrapMode GL.Texture2D GL.S))
                             (forM_ [GL.S,GL.T] . aux)
  where aux x d = GL.textureWrapMode GL.Texture2D d $= x

--------------------

-- |
drawing :: Image PixelRGBA8
drawing = Rasterific.renderDrawing 256 256 white
        . Rasterific.withTexture (Rasterific.uniformTexture drawColor)
        . Rasterific.fill
        $ Rasterific.circle (V2 0 0) 140
  where
    white = PixelRGBA8 255 255 255 255
    drawColor = PixelRGBA8 0 0x86 0xc1 255