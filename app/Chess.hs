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


-- API -------------------------------------------------------------------------

module Main where

-- We'll need these ------------------------------------------------------------

-- *
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

-- *

-- *
import Graphics.UIKit as UI

-- Definitions -----------------------------------------------------------------

-- |
data Index = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Show, Eq, Ord, Enum, Bounded)

-- |
data ChessColour = White | Black deriving (Show, Eq, Enum, Bounded)

-- |
data _ = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show, Enum, Bounded)

-- |
data ChessPiece = ChessPiece !_ !ChessColour

-- |
newtype ChessBoard = ChessBoard (Map (V2 Index) (Maybe ChessPiece))

--------------------------------------------------------------------------------

initial :: String
initial = unlines ["♖♘♗♕♔♗♘♖",
					         "♙♙♙♙♙♙♙♙",
					         "        ",
					         "        ",
					         "        ",
					         "        ",
					         "♟♟♟♟♟♟♟♟",
					         "♜♞♝♛♚♝♞♜"]


white = "♜♞♝♛♚♟" -- All white pieces
black = "♖♘♗♕♔♙" -- All black pieces

--------------------------------------------------------------------------------

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
fromGlyph _ = Nothing

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
main :: IO ()
main = runChess


-- |
runChess :: IO ()
runChess = runApplication
  (\scene game -> renderChess)
  (\msg old    -> updateChess)
  (\initial    -> newChess _)