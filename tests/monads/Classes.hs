module Classes where

import Prelude (const, id, ($), String)

-- A reasonably conservative Functor-Applicative-Monad hierarchy.

infixl 4 <*>, <*, *>
infixl 4 <$>

class Functor f where
  map :: (a -> b) -> f a -> f b

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a

  a *> b = map (const id) a <*> b
  a <* b = map const a <*> b
 
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  x >>= f = join $ map f x

  join :: m (m a) -> m a
  join x = x >>= id
 
class Monad m => MonadFail m where
  fail :: String -> m a

return :: Monad m => a -> m a
return = pure

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = map

liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b