-- |
-- Module      : Main
-- Description : The classic game like you've never experiences it before.
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : $
-- Portability : $
--

-- TODO | - Refactor (when it's done)
--        - Multiplayer (?)

-- GHC Directives  -------------------------------------------------------------

{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving     #-}

-- API -------------------------------------------------------------------------

module Main where

-- We'll need these ------------------------------------------------------------

import qualified Data.Vector          as V
import           Data.Vector          (Vector, (!?))
import           Data.Fixed (mod', div')
import           Data.Maybe
import           Data.Monoid
import           Data.Foldable

import Text.Printf

import Linear
import Lens.Micro.Platform

-- *
import Control.Monad

-- *
import           Graphics.Rasterific         as Rasterific hiding (Vector)
import           Graphics.Rasterific.Texture as Rasterific
import qualified Graphics.Text.TrueType as Font
import           Graphics.Text.TrueType (Font(..), Dpi(..), BoundingBox(..))
import           Graphics.UI.GLFW (MouseButton(..), Key(..), KeyState(..), MouseButtonState(..), CursorState(..))

import Codec.Picture (Image(..), PixelRGB8(..), PixelRGBA8(..), DynamicImage(..), savePngImage)

-- *
import Data.AABB as AABB

-- *
import Control.Loops
import Data.Chroma as Chroma

-- *
import Graphics.UIKit.Types
import Graphics.UIKit.Lenses
import Graphics.UIKit

-- Definitions -----------------------------------------------------------------

-- TODO | - Factor out to demo executable
--        -

-- |
data Tile = Nought | Cross deriving (Eq, Show, Bounded, Enum)


-- |
data Outcome = AlreadyOccupied (V2 Int)
             | PlacedTile GameStatus Tile (V2 Int)
             deriving (Eq, Show)


-- |
data GameStatus = Impasse
                | Ended Tile [V2 Int]
                | Ongoing
                deriving (Eq, Show)


-- |
-- TODO | - Polymorphic, n-dimensional (?)
--        - Nice 'Grid' representation (eg. Map indexed by Bounded Enum type)
data TicTacToe = TicTacToe {
  fBoard   :: Vector (Maybe Tile), -- TODO | - Size-contrained sequeneces
  fSide    :: Int,                 -- The length of each side
  fTurns   :: [Tile],              --
  fHistory :: [Outcome],           --
  fStatus  :: GameStatus,          --
  fLayout  :: BoardLayout,         --
  fInput   :: Input
} deriving (Show)


-- |
data BoardLayout = BoardLayout {
  fOrigin   :: V2 Float,
  fTileSize :: V2 Float,
  fPadding  :: V2 Float,
  fSide     :: Int -- TODO | - Duplicated data, remove
} deriving (Show)

--------------------------------------------------------------------------------

---- * Grid
--
---- |
--data Square a = Square {
--  fSide  :: !Int,
--  fItems :: !(Vector a)
--} deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | Lenses for all
--concat <$> mapM (makeLensesWith abbreviatedFields) [''TicTacToe, ''BoardLayout]
makeLensesWith abbreviatedFields ''TicTacToe
makeLensesWith abbreviatedFields ''BoardLayout


-- |
-- TODO | - Turn into prism (deal with out-of-bounds)
--        - Converting a position to a linear index can sometimes hide out-of-bonds errors
tileAt :: V2 Int -> Lens' TicTacToe (Maybe Tile)
tileAt pos f s = let i = toIndex (s~>side) pos in (\new -> s & board.ix i .~ new) <$> f (join $ fBoard s !? i)


-- | Let's not undo previous moves. The game would take forever.
unlessOccupied :: Lens' (Maybe Tile) (Maybe Tile)
unlessOccupied f s@Nothing = const s <$> f s
unlessOccupied f s         = f s


-- | Focuses on the tile whose turn it is
-- TODO | - Rename (?)
current :: SimpleGetter TicTacToe Tile
current = turns.to head


-- | The number of moves that 'tile' has made
nmoves :: Tile -> SimpleGetter TicTacToe Int
nmoves tile = to $ \game -> countBy (== Just tile) (game~>board)


-- |
-- TODO | - Factor out
countBy :: Foldable t => (a -> Bool) -> t a -> Int
countBy f = foldr (\x c -> c + fromEnum (f x)) 0


-- | 
-- TODO | - This will break if we add more dimensions
consecutives :: Int -> [[V2 Int]]
consecutives s = horizontals <> verticals <> diagonals
  where
    hi = s-1
    horizontals = fmap (\y -> [V2 x y | x <- [0 .. hi]]) [0 .. hi]
    verticals   = fmap (\x -> [V2 x y | y <- [0 .. hi]]) [0 .. hi]
    diagonals = [[V2 (xy)    xy | xy <- [0 .. hi]],
                 [V2 (hi-xy) xy | xy <- [0 .. hi]]]


-- |
tileChar :: Tile -> Char
tileChar Nought = 'O'
tileChar Cross  = 'X'


-- |
opponent :: Tile -> Tile
opponent Nought = Cross
opponent Cross  = Nought


-- | The (V2 col row) position of each tile, ordered by the index of each position.
-- TODO | - Current logic will break if we ever generalise to other dimensions
positions :: Int -> [V2 Int]
positions s = [V2 col row | row <- [0 .. (s-1)], col <- [0 .. (s-1)]]


-- |
-- TODO | - Rename
tileLayout :: BoardLayout -> [AABB V2 Float]
tileLayout layout = fmap (tileBounds layout) (positions $ layout~>side)


-- |
-- TODO | - Out-of-bounds
tileBounds :: BoardLayout -> V2 Int -> AABB V2 Float
tileBounds layout pos = let sz = layout~>tileSize
                            or = layout~>origin
                            lo = or + (sz + layout~>padding) * (fromIntegral <$> pos)
                        in AABB lo (lo + sz)


-- | Transforms a coordinate in screen space to a position on the board.
-- TODO | - Refactor
toBoardPosition :: BoardLayout -> V2 Float -> Maybe (V2 Int)
toBoardPosition layout coord
  | not . and $ (\n hi -> between n 0 hi)          <$> normed <*> boardSize  = Nothing
  | not . and $ (\n sz pd -> mod' n (sz+pd) <= sz) <$> normed <*> sz <*> pad = Nothing
  | otherwise = Just $ div' <$> normed <*> padded
  where
    sz  = layout~>tileSize
    pad = layout~>padding
    normed    = coord - (layout~>origin)
    padded    = layout~>tileSize + layout~>padding -- Size of a single tile, with padding included
    boardSize = padded * pure (layout~>side.to fromIntegral) - (layout~>padding)
    between n a b = (a <= n) && (n <= b)


-- | Transforms a position on the board into a linear index.
toIndex :: Int -> V2 Int -> Int
toIndex s (V2 px py) = px + s * py


-- |
tryPlace :: TicTacToe -> V2 Int -> Outcome
tryPlace game pos
  | isNothing here = PlacedTile status current pos
  | otherwise      = AlreadyOccupied pos
  where
    here = join $ (game~>board) !? toIndex (game~>side) pos
    current = game~>turns.to head
    status
      -- NOTE | - The board goes out of sync for a short while, the status logic has to 'look ahead' by inserting the new value
      -- TODO | - Refactor
      | all isJust ((game & tileAt pos .~ Just current)~>board) = Impasse
      | otherwise                = case find (all (\p -> (== Just current) $ (game & tileAt pos .~ Just current)~>tileAt p)) (consecutives $ game~>side) of
                                     Just match -> Ended current match
                                     Nothing    -> Ongoing


-- |
applyOutcome :: TicTacToe -> Outcome -> TicTacToe
applyOutcome game o@(AlreadyOccupied _)      = game & history %~ (o:)
applyOutcome game o@(PlacedTile st tile pos) = game & history %~ (o:)
                                                    & turns %~ drop 1
                                                    & status .~ st
                                                    & board.ix (toIndex (game~>side) pos) .~ Just tile


-- | Play the game. Save the world.
update :: SystemEvent -> TicTacToe -> TicTacToe
update (KeyDown Key'Space) game          = newGame (game~>input) 3
update ev game@((~>status) -> Impasse)   = game
update ev game@((~>status) -> Ended c m) = game
update ev game = case ev of
  MouseDown MouseButton'1 -> maybe game (\pos -> applyOutcome game $ tryPlace game pos) $ toBoardPosition (game~>layout) cursorOnCanvas
  _                       -> game
  where
    cursorOnCanvas = game~>input.mouse.cursor.to (fmap realToFrac)

--------------------------------------------------------------------------------

--class UIComponent a where
--  renderUI :: a -> Drawing PixelRGBA8 ()
--  boundsUI :: a -> AABB V2 Int

data UIComponent = UIComponent {
  renderUI :: UIComponent -> Drawing PixelRGBA8 (),
  boundsUI :: AABB V2 Int
}

--------------------------------------------------------------------------------

-- |
-- TODO | - Don't re-render tiles or text (cf. highlighted, etc.)
--        - 
drawing :: Scene -> TicTacToe -> Drawing PixelRGBA8 ()
drawing scene game = case (game~>status) of
    Ongoing       -> renderAllTiles >> renderAllLabels >> solid (PixelRGBA8 20 140 240 255) renderPendingChoice >> ongoingUI
    Ended won row -> renderAllTiles >> renderWinningTiles row >> renderAllLabels >> winningUI won row
    Impasse       -> impasseUI
  where
    -- TODO | - I should really finish up 'Interpolate'
    winningUI won row = solid green $ anchoredText (scene~>to font) (96) (PointSize 32) (winMessage won row) (pure 0.5) frameCentre
    ongoingUI = pass
    impasseUI = solid black $ anchoredText (scene~>to font) (96) (PointSize 48) ("I M P A S S E") (pure 0.5) frameCentre
    renderPendingChoice = maybe pass (\(pos, tile) -> tileLabel (const $ Just tile) pos) findPendingChoice

    winMessage won _ = printf "%s won after %d moves" (show won) (game~>nmoves won)

    findPendingChoice :: Maybe (V2 Int, Tile)
    findPendingChoice = do pos  <- find (inside cursorOnCanvas . bounds) (positions $ game~>side)
                           tile <- maybe (game~>current.to Just) (const Nothing) (game~>tileAt pos)
                           return (pos, tile)

    renderAllTiles  = solid (PixelRGBA8 50 100 200 255) . void $ mapM tileFill (positions $ game~>side)
    renderAllLabels = solid (PixelRGBA8 40 40 40 255) $ mapM_ (tileLabel $ \pos -> game~>tileAt pos) (positions (game~>side))
    renderWinningTiles row = solid (PixelRGBA8 183 240 183 255) . void $ mapM tileFill row
    
    solid c = withTexture (uniformTexture c)
    
    bounds = tileBounds (game~>layout)
    tileFill  pos = fill $ aabb (bounds pos)
    tileLabel f pos = let label  = pure . tileChar
                          mtile  = f pos
                          box    = bounds pos
                          anchor = V2 0.5 0.5
                          p      = (box~>lo) + (box~>size)*(V2 0.5 0.5)
                      in maybe pass (\tile -> anchoredText (scene~>to font) 96 (PointSize 54) (label tile) anchor p) mtile
    
    cursorOnCanvas = game~>input.mouse.cursor.to (fmap realToFrac)
    frameCentre    = game~>input.frameSize.asFloat.to (* pure 0.5)
    asFloat = to (fmap fromIntegral)

--------------------------------------------------------------------------------

-- * Borrowed from Pixels

-- TODO | - Factor out

-- |
-- TODO | - Sort out the terminology, rename parameters
--        - Factor out
--        - Baseline height (?)
anchoredTo :: Font -> Dpi -> PointSize -> String -> V2 Float -> V2 Float -> V2 Float
anchoredTo font dpi pt s anchor p = let box = stringBounds font dpi pt s in p - anchor * (box~>size) {- SW to NW, TODO | - More general solution -} + V2 0 (box~>size.y)


-- |
anchoredText :: Font -> Dpi -> PointSize -> String -> V2 Float -> V2 Float -> Drawing px ()
anchoredText font dpi pt s anchor p = let lo = anchoredTo font dpi pt s anchor p in printTextAt font pt lo s


-- |
toCartesianBox :: BoundingBox -> AABB V2 Float
toCartesianBox box@(BoundingBox x₀ y₀ x₁ y₁ _) = AABB (V2 x₀ y₀) (V2 x₁ y₁)


-- |
stringBounds :: Font -> Dpi -> PointSize -> String -> AABB V2 Float
stringBounds font dpi pt = toCartesianBox . Font.stringBoundingBox font dpi pt

--------------------------------------------------------------------------------

-- |
newGame :: Input -> Int -> TicTacToe
newGame initial sideLength = TicTacToe {
                               fBoard   = V.replicate (sideLength^2) Nothing,
                               fSide    = sideLength,
                               fTurns   = cycle [Nought, Cross],
                               fHistory = [],
                               fStatus  = Ongoing,
                               fInput   = initial,
                               fLayout  = BoardLayout {
                                            fOrigin   = V2 20 20,
                                            fTileSize = V2 105 105,
                                            fPadding  = V2 6 6,
                                            fSide     = sideLength } }


-- |
runTicTacToe = runApplication
                 (\scene game -> let (V2 dx dy) = game^.input.frameSize in renderDrawing dx dy Chroma.white $ drawing scene game)
                 (\msg old -> update msg $ old { fInput = onevent msg (old~>input) })
                 (\initial -> newGame initial 3)


-- |
main :: IO ()
main = runTicTacToe >>= either (putStrLn) (\_ -> putStrLn "Hurrah!")