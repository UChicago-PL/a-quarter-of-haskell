-- | A greatly simplified version of Control.Monad.Reader, which implements only the basic
--   Reader type, along with its related functions.

module Writer where

import Data.Monoid ((<>))

newtype Writer w a = Writer { runWriter :: (a,w) }

instance Functor (Writer w) where
    fmap f (Writer (a,w)) = Writer (f a, w)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a,mempty)
    (Writer (fa,fw)) <*> (Writer (xa,xw)) = Writer (fa xa, fw <> xw)


-- =============================================================================
-- STUB: Fill in the following:

instance Monoid w => Monad (Writer w) where
    Writer (xa,xw) >>= f = undefined
-- =============================================================================

-- | Construct a writer action.

writer :: (w,a) -> Writer w a
writer (w,a) = Writer (a,w)

-- | Construct a writer action with a given message.

tell :: w -> Writer w ()
tell w = Writer ((),w)

-- | Expose the hidden message in the value of a writer action.

listen :: Writer w a -> Writer w (a,w)
listen (Writer (a,w)) = Writer ((a,w),w)

-- | An <*>-like function for the message component of writer actions.

pass :: Writer w (a,w->w) -> Writer w a
pass (Writer ((a,f),w)) = Writer (a,f w)

-- | Expose the result of applying a function to the hidden message of a writer action.

listens :: Monoid w => (w -> b) -> Writer w a -> Writer w (a,b)
listens f (Writer (a,w)) = Writer ((a,f w),w)

-- | An fmap-like function that acts on the message component of a writer action.

censor :: Monoid w => (w -> w) -> Writer w a -> Writer w a
censor f (Writer (a,w)) = Writer (a,f w)

