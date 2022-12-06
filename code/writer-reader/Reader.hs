-- | A greatly simplified version of Control.Monad.Reader, which implements only the basic
--   Reader type, along with its related functions.

module Reader where

newtype Reader e a = Reader { runReader :: e -> a }

instance Functor (Reader e) where
    fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader e) where
    pure a = Reader $ const a
    Reader f <*> Reader x = Reader $ \e -> f e (x e)

instance Monad (Reader e) where
    Reader x >>= f = Reader $ \e ->
        let Reader r = f (x e)
        in r e

-- | Retrieve the environment.

ask :: Reader e e
ask = Reader id

-- | Run an action in a modified environment.

local :: (e -> e) -> Reader e a -> Reader e a
local f (Reader g) = Reader $ g . f

-- | Retrieve a function applied to the current environment.

asks  :: (e -> a) -> Reader e a
asks f = Reader f
