{- A pedagogical implementation of the standard State monad -}

module State where

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
    fmap f ma = State $ \s ->
        let (a,t) = runState ma s
        in (f a,t)

instance Applicative (State s) where
    pure a = State $ \s -> (a,s)
    af <*> aa = State $ \s ->
        let (f,t) = runState af s
            (a,u) = runState aa t
        in (f a, u)

instance Monad (State s) where
    return = pure
    ma >>= f = State $ \s ->
        let (a,t) = runState ma s
            mb = f a
            (b,u) = runState mb t
        in (b,u)

{- constructor -}

state :: (s -> (a,s)) -> State s a
state = State

{- primitive state manipulation functions -}

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put t = State $ \s -> ((),t)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((),f s)

{- evaluation of state values -}

evalState :: State s a -> s -> a
evalState ma s = fst (runState ma s)

execState :: State s a -> s -> s
execState ma s = snd (runState ma s)
