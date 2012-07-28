{-# LANGUAGE TemplateHaskell, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

module Templates where

import qualified Classes as D

import Prelude ( (=<<), String )

import Language.Haskell.InstanceTemplates
import Language.Haskell.TH.Syntax -- This is just for prettier -ddump-splices

-- The instance templates are defined in a separate module because they use new
-- names

-- TODO: use nested instantiations once they work.

type Functor f = D.Functor f

$(mkTemplate =<< [d|
  class Functor f where
    fmap :: (a -> b) -> f a -> f b
  instance D.Functor f where
    map = fmap
 |] )

{- Instance templates syntax

deriving class Functor f where
  fmap :: (a -> b) -> f a -> f b
  instance D.Functor f where
    map = fmap

-}


type Applicative f = (D.Functor f, D.Applicative f)

$(mkTemplate =<< [d|
  class Applicative f where
    pure   :: a -> f a
    (<*>)  :: f (a -> b) -> f a -> f b
    (*>)   :: f a -> f b -> f b
    (<*)   :: f a -> f b -> f a
    {- TODO
    a <* b = const id <$> a <*> b
    a *> b = const    <$> a <*> b
    -}
  
  instance D.Functor f where
    map f x = pure f <*> x
  
  instance D.Applicative f where
    pure  = pure
    (<*>) = (<*>)
    (*>)  = (*>)
    (<*)  = (<*)
 |] )

type Monad a = D.Monad a

$(mkTemplate =<< [d|
  class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    (>>) :: m a -> m b -> m b
    return :: a      -> m a

    {- TODO
    m >> k = m >>= \_ -> k
    fail = error
    -}

  -- This means that "fail" parameter is inherited form this instance.
  instance Inherit (Instance (D.MonadFail m)) where

  instance D.Functor m where
    map f x = D.pure f D.<*> x

  instance D.Applicative m where
    pure = return
    l <*> r = l >>= \f ->
              r >>= \x -> return (f x)

  instance D.Monad m where
    (>>=) = (>>=)
    (>>)  = (>>)
 |] )