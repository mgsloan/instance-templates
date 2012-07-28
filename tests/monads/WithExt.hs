{-# LANGUAGE InstanceTemplates, RebindableSyntax #-}

-- | This is a sketch of what this code would look like with -XInstanceTemplates
import qualified Classes as D

import Prelude (Eq, Read, Show, (+), (-), zipWith, repeat, ($), const, id)


deriving class Functor f where
  fmap :: (a -> b) -> f a -> f b
  instance D.Functor f where
    map = fmap

deriving class Applicative f where
  pure   :: a -> f a
  (<*>)  :: f (a -> b) -> f a -> f b
  (*>)   :: f a -> f b -> f b
  (<*)   :: f a -> f b -> f a

  a <* b = const id <$> a <*> b
  a *> b = const    <$> a <*> b
  
  instance D.Functor f where
    map f x = pure f <*> x
  
  instance D.Applicative f where
    pure  = pure
    (<*>) = (<*>)
    (*>)  = (*>)
    (<*)  = (<*)

deriving class Monad m where
  -- Note: this means that the methods of D.Monad and D.MonadFail are provided
  -- as parameters of the template.
  instance D.MonadFail m

  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a      -> m a

  m >> k = m >>= \_ -> k
  fail = error

  instance D.Functor m where
    map f x = D.pure f D.<*> x

  instance D.Applicative m where
    pure = return
    l <*> r = l >>= \f ->
              r >>= \x -> return (f x)

  instance D.Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b


data Maybe a = Just a | Nothing
  deriving (Eq, Read, Show)

instance Monad Maybe where
  Just x >>= k  =  k x
  Nothing >>= _ =  Nothing

  Just _ >> k   =  k
  Nothing >> _  =  Nothing

  return x      =  Just x

  fail _        =  Nothing

testMaybe = (-) <$> Just 49 <*> Just 7


newtype ZipList a = ZipList [a]
  deriving (Eq, Read, Show)

instance Applicative (ZipList a) where
-- instance Applicative ZipList where
    (ZipList fs) <*> (ZipList xs) = ZipList (zipWith ($) fs xs)
    pure x                        = ZipList (repeat x)

    (*>) :: D.Applicative f => f a -> f b -> f b
    (*>) = D.liftA2 (const id)
    (<*) :: D.Applicative f => f a -> f b -> f a
    (<*) = D.liftA2 const

testZipList = (+) <$> ZipList [1,2,3] <*> ZipList [6,5,4]