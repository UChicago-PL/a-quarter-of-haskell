{- NaturalNumber.hs

   A partial Haskell implementation of the natural numbers via the Peano-Dedekind
   Axioms for arithmetic, with a unary internal representation.
 -}

module NaturalNumber where

data NaturalNumber = Zero
                   | S NaturalNumber
    deriving (Show)

-- common names for small natural numbers

zero  = Zero
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven
nine  = S eight
ten   = S nine

-- and two possibly unexpected elements of NaturalNumber

infinity = S infinity

loop :: NaturalNumber
loop = loop

instance Eq NaturalNumber where
    Zero == Zero = True
    Zero == S y  = False
    S x  == Zero = False
    S x  == S y  = x == y

instance Ord NaturalNumber where
    compare Zero Zero   = EQ
    -- writing the other cases of this forms exercise 2.1
        
instance Num NaturalNumber where
    x + Zero = x
    x + S y  = S (x + y)
    
    x * Zero = Zero
    x * S y  = x + x * y
    
    fromInteger n
        | n > 0  = S (fromInteger (n-1))
        | n == 0 = Zero
   
    -- the implementations of abs and signum form exercise 2.2
    
    abs = undefined
    signum = undefined

nat :: NaturalNumber -> NaturalNumber
nat = id
