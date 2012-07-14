{-# LANGUAGE TemplateHaskell, QuasiQuotes, ConstraintKinds, ScopedTypeVariables, FlexibleInstances #-}

module Templates where

import qualified Classes as C
import Language.Haskell.InstanceTemplates

import Language.Haskell.TH.Syntax -- This is just for prettier -ddump-splices

-- The instance templates are defined in a separate module because they use new
-- names

-- TODO: use nested instantiations once they work.

type Functor f = C.Functor f

$(mkTemplate =<< [d|
  class Functor f where
    fmap :: a -> f a
  instance C.Functor f where
    map = fmap
 |] )

type Applicative f = (C.Functor f, C.Applicative f)

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
  
  instance C.Functor f where
    map f x = pure f <*> x
  
  instance C.Applicative f where
    pure  = pure
    (<*>) = (<*>)
    (*>)  = (*>)
    (<*)  = (<*)
 |] )

{-
deriving class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a ->       m b  -> m b
  return :: a      -> m a
  fail   :: String -> m a

  m >> k = m >>= \_ -> k
  fail = error

  -- Note! This instantiates the template above
  instance Applicative m where
    return = return
    l <*> r = l >>= \f ->
              r >>= \x -> return (f x)

  instance C.Monadic m where
    (>>=) = (>>=)

  instance C.MonadFail m where
    fail = fail
-}