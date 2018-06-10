module Failable where

import Control.Applicative (Applicative(..))
import Control.Monad       (liftM, ap)

--------------------------------------------------------------------------------

-- Representa un resultado que puede ser correcto o fallído con su mensaje de error
data Failable m = Error String
                | Ok m
                deriving Show

instance Monad Failable where
    return x = Ok x
    (Error err) >>= f = Error err
    (Ok x) >>= f = f x

-- Para calmar al GHC
instance Functor Failable where
    fmap = liftM

instance Applicative Failable where
    pure   = return
    (<*>)  = ap
