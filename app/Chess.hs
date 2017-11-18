-- |
-- Module      : Main
-- Description : The robots have us beat, but some people still enjoy it
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : $
-- Portability : $
--

-- TODO | - 
--        - 

-- GHC Pragmas -----------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE OverloadedRecordFields #-}

-- API -------------------------------------------------------------------------

module Main where

-- We'll need these ------------------------------------------------------------

-- *
import           Data.Monoid
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Maybe (isJust)

import Control.Monad.Trans.Either
import Control.Monad

-- *
import Lens.Micro.Platform

-- *
import           Graphics.Rasterific         as Rasterific hiding (Vector)
import           Graphics.Rasterific.Texture as Rasterific
import qualified Graphics.Text.TrueType as Font
import           Graphics.Text.TrueType (Font(..), Dpi(..), BoundingBox(..))
import           Graphics.UI.GLFW (MouseButton(..), Key(..), KeyState(..), MouseButtonState(..), CursorState(..))

import Codec.Picture (Image(..), PixelRGB8(..), PixelRGBA8(..), DynamicImage(..), savePngImage)

import Wuss

-- *
import Data.AABB as AABB

-- *
import Data.Chroma as Chroma

import Graphics.UIKit.Types
import Graphics.UIKit.Lenses
import Graphics.UIKit as UI

-- Definitions -----------------------------------------------------------------

-- |
data Index = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Show, Eq, Ord, Enum, Bounded)

-- |
data ChessColour = White | Black deriving (Show, Eq, Enum, Bounded)

-- |
-- TODO | - Rename (?)
--        - 
data ChessPieceKind = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show, Enum, Bounded)

-- |
data ChessPiece = ChessPiece !ChessPieceKind !ChessColour

-- |
newtype ChessBoard = ChessBoard (Map (V2 Index) ChessPiece)

-- |
data ChessLayout = ChessLayout {
  fTileSize :: V2 Float,
  fTilePad  :: V2 Float,
  fOrigin   :: V2 Float
} deriving (Show)
  
  -- |
data ChessGame = ChessGame {
  fBoard  :: ChessBoard,
  fTurns  :: [ChessColour],
  fLayout :: ChessLayout,
  fInput  :: Input,
  fPieceFont :: Font
}

--------------------------------------------------------------------------------

makeLensesWith abbreviatedFields ''ChessGame
makeLensesWith abbreviatedFields ''ChessLayout

--------------------------------------------------------------------------------

-- | The initial formation of the pieces on the board
-- TODO | - Rename (?)
--        - 
formation :: ChessBoard
formation = ChessBoard . Map.fromList $ makeFormationRow White Eight (rearGuard)        <>
                                        makeFormationRow White Seven (reverse vanGuard) <>
                                        makeFormationRow Black Two   (vanGuard)         <>
                                        makeFormationRow Black One   (rearGuard)
  where
    makeFormationRow colour y pieces = zipWith (\x p -> (V2 x y, ChessPiece p colour)) universe pieces

-- | The frontline consists of eight brave pawns, equipped with pitchforks and saucepans
vanGuard :: [ChessPieceKind]
vanGuard = [Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn, Pawn]

-- | The rear guard is more powerful and well-equpped. The order should be reversed for the black lineup.
rearGuard :: [ChessPieceKind]
rearGuard = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

-- | The index of every position on the board, starting from the lower left corner (V2 One One)
--   and scanning the lines left to right.
positions :: [[V2 Index]]
positions = [[V2 col row | col <- universe] | row <- universe]

-- | Enumerate every single value of a 'Bounded' type
-- TODO | - Rename (eg. 'enumerate') (?)
--        - 
universe :: (Enum a, Bounded a) => [a]
universe = [minBound .. maxBound]

-- |
lookupGlyph :: ChessBoard -> V2 Index -> Char
lookupGlyph (ChessBoard board) pos = maybe ' ' toGlyph $ Map.lookup pos board

-- |
showBoard :: ChessBoard -> String
showBoard board = unlines . reverse $ fmap (fmap $ lookupGlyph board) positions

--------------------------------------------------------------------------------

-- | Finds every possible move for a given 'ChessPiece', relative to the current
--   position.
--   TODO | - Generate 'paths' ()
{- moves :: V2 Int -> ChessPiece -> [V2 Int]
moves pos piece = case piece of
  Pawn   -> _
  Knight -> _
  Bishop -> _
  Rook   -> _
  Queen  -> _
  King   -> _
  Pawn   -> _
  Knight -> _
  Bishop -> _
  Rook   -> _
  Queen  -> _
  King   -> _ -}

--------------------------------------------------------------------------------

-- |
pieceAt :: ChessBoard -> V2 Index -> Maybe ChessPiece
pieceAt (ChessBoard board) pos = Map.lookup pos board

-- |
fromGlyph :: Char -> Maybe ChessPiece
fromGlyph '♟' = Just $ ChessPiece Pawn   White
fromGlyph '♞' = Just $ ChessPiece Knight White
fromGlyph '♝' = Just $ ChessPiece Bishop White
fromGlyph '♜' = Just $ ChessPiece Rook   White
fromGlyph '♛' = Just $ ChessPiece Queen  White
fromGlyph '♚' = Just $ ChessPiece King   White
fromGlyph '♙' = Just $ ChessPiece Pawn   Black
fromGlyph '♘' = Just $ ChessPiece Knight Black
fromGlyph '♗' = Just $ ChessPiece Bishop Black
fromGlyph '♖' = Just $ ChessPiece Rook   Black
fromGlyph '♕' = Just $ ChessPiece Queen  Black
fromGlyph '♔' = Just $ ChessPiece King   Black
fromGlyph _    = Nothing

-- |
toGlyph :: ChessPiece -> Char
toGlyph (ChessPiece Pawn   White) = '♟'
toGlyph (ChessPiece Knight White) = '♞'
toGlyph (ChessPiece Bishop White) = '♝'
toGlyph (ChessPiece Rook   White) = '♜'
toGlyph (ChessPiece Queen  White) = '♛'
toGlyph (ChessPiece King   White) = '♚'
toGlyph (ChessPiece Pawn   Black) = '♙'
toGlyph (ChessPiece Knight Black) = '♘'
toGlyph (ChessPiece Bishop Black) = '♗'
toGlyph (ChessPiece Rook   Black) = '♖'
toGlyph (ChessPiece Queen  Black) = '♕'
toGlyph (ChessPiece King   Black) = '♔'

--------------------------------------------------------------------------------

-- |
tileBounds :: ChessLayout -> V2 Index -> AABB V2 Float
tileBounds layout pos = let o   = layout~>origin
                            off = fmap (fromIntegral . fromEnum) pos * (layout~>tileSize + layout~>tilePad)
                            sz  = layout~>tileSize
                        in AABB.fromCornerSize (o + off) sz

-- |
tilesLayout :: ChessLayout -> [AABB V2 Float]
tilesLayout layout = fmap (tileBounds layout) (mconcat positions)

--------------------------------------------------------------------------------

-- |
renderChess :: Scene -> ChessGame -> Image PixelRGBA8
renderChess scene game = renderDrawing (game~>input.frameSize.x) (game~>input.frameSize.y) Chroma.white $ do
  solid (rgb  50 100 200) . void $ mapM (fill . aabb) (tilesLayout $ game~>layout)
  --solid (rgb 255 255 255) . void $ mapM (\pos -> printTextAt (game~>pieceFont) pt (tileBounds (game~>layout) pos~>lo) (glyphAt pos)) (mconcat positions)
  --solid (rgb 0 0 0) . mapM_ (stroke 1 JoinRound (CapRound,CapRound) . aabb . tileBounds (game~>layout))
  mapM_ renderPiece . filter (isJust . pieceAt (game~>board)) $ mconcat positions
  where
    --chequer|(s,ed)
    dpi = 96
    pt  = PointSize 20

    glyphAt = pure . maybe ' ' toGlyph . pieceAt (game~>board)
    textbox pos = let lo = anchoredTo (game~>pieceFont) dpi pt glyph (V2 0.5 0.5) (tileBounds (game~>layout) pos~>centre)
                      glyph = glyphAt pos
                      box   = stringBounds (game~>pieceFont) dpi pt $ glyph
                  in box & position %~ (+ (lo + 2 - (box~>height.to (V2 (2)))))

    renderPiece pos = let box    = tileBounds (game~>layout) pos
                          font   = game~>pieceFont
                          bounds = let b = stringBounds font dpi pt text in b & position %~ (+ ((box~>lo) - (b~>height.to (V2 0))))
                          text   = glyphAt pos
                      in do solid (rgb 0 0 0) $ printTextAt font pt (box~>lo) text
                            solid (rgb 0 0 0) . line $ aabb bounds

    rgb r g b = PixelRGBA8 r g b maxBound
    solid c = withTexture (uniformTexture c)
    line = stroke 1 JoinRound (CapRound,CapRound)
    drawGlyphAt pos = anchoredText
                        (game~>pieceFont)
                        (dpi)
                        (pt)
                        (glyphAt pos)
                        (V2 0.5 0.5)
                        (tileBounds (game~>layout) pos~>centre)

-- |
updateChess :: SystemEvent -> ChessGame -> ChessGame
updateChess _ game = game

-- |
newChess :: Input -> EitherT String IO ChessGame
newChess initial = ChessGame
                     <$> pure (formation)
                     <*> pure (cycle [White, Black])
                     <*> pure (ChessLayout { fTileSize = V2 50 50, fTilePad = V2 4 4, fOrigin = V2 15 15 })
                     <*> pure (initial)
                     <*> (EitherT $ Font.loadFontFile "assets/fonts/ARIALUNI.TTF")

--------------------------------------------------------------------------------

-- |
runChess :: IO (Either String ())
runChess = runApplication
             ("Chess")
             (V2 650 650)
             (\scene game -> renderChess scene game)
             (\msg old    -> updateChess msg old)
             (\initial    -> newChess initial)

-- |
main :: IO ()
main = runChess >>= either putStrLn (const $ putStrLn "Hurrah!")