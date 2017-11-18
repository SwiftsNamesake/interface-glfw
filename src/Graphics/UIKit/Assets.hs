-- |
-- Module      : Graphics.UIKit.Assets
-- Description : 
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : $
-- Portability : $
--

-- TODO | - Use Cabal's data-files
--        - 

-- GHC Pragmas -----------------------------------------------------------------

{-# LANGUAGE TemplateHaskell #-}

-- API -------------------------------------------------------------------------

module Graphics.UIKit.Assets where

-- We'll need these ------------------------------------------------------------

import Data.ByteString
import Data.FileEmbed

import Language.Haskell.TH.Syntax (Q(..), Lit(..), Exp(..))

-- Definitions -----------------------------------------------------------------

-- |
-- TODO | - This isn't really robust, as it ties the path to the computer we compile on.
robustPath :: FilePath -> Q Exp
robustPath = fmap (LitE . StringL) . makeRelativeToProject

-- |
embed :: FilePath -> Q Exp
embed fn = makeRelativeToProject fn >>= embedFile