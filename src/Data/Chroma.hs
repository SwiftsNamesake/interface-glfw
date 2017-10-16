-- |
-- Module      : Data.Chroma
-- Description : All the colours
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : $
-- Portability : $
--

-- TODO | - 
--        - 

-- API -------------------------------------------------------------------------

module Data.Chroma where

-- We'll need these ------------------------------------------------------------

import Codec.Picture (PixelRGB8(..), PixelRGBA8(..))

-- Definitions -----------------------------------------------------------------

black = PixelRGBA8   0   0   0 255
white = PixelRGBA8 255 255 255 255
red   = PixelRGBA8 255   0   0 255
green = PixelRGBA8   0 255   0 255
blue  = PixelRGBA8   0   0 255 255