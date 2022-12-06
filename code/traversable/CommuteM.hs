-- | A demonstration that if monads commuted, then the composition of
--   monads would be a monad.

module CommuteM where

import Control.Monad
import Data.Functor.Compose

-- | We undefine the undefinable.

commuteM :: (Monad m, Monad n) => m (n a) -> n (m a)
commuteM = undefined

-- | Use the undefinable to implement an impossible natural join for
--   composed monads.

joinC :: (Monad m, Monad n)
      => Compose m n (Compose m n a)
      -> Compose m n a
joinC = Compose
      . (join <$>)
      . join
      . (commuteM <$>) 
      . getCompose
      . (getCompose <$>)

-- | Use the impossible join for composed monads to create the
--   impossible Monad instance for composed monads.

instance (Monad m, Monad n) => Monad (Compose m n) where
    mna >>= f = joinC (f <$> mna)
