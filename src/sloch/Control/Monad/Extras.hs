module Control.Monad.Extras where

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb a1 a2 = mb >>= \b -> if b then a1 else a2

-- | Like when, but conditional on a Maybe being Just rather than a Bool being True.
whenMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenMaybe = flip $ maybe $ return ()
