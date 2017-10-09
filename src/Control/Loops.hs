-- |
-- Module      : Control.Loops
-- Description : When and while just aren't enough
-- Copyright   : (c) Jonatan Sundqvist, 2017
-- License     : MIT
-- Maintainer  : Jonatan Sundqvist
-- Stability   : $
-- Portability : $
--

-- TODO | - 
--        - 

-- API -------------------------------------------------------------------------

module Control.Loops where

-- We'll need these ------------------------------------------------------------

import Control.Monad.Trans.Either

-- Definitions -----------------------------------------------------------------

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
