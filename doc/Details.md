Inheriting Methods
==================

In order to conveniently express things like the [Class Alias Proposal]
(http://repetae.net/recent/out/classalias.html) and Superclass Default
Instances, the following shorthand is provided:

```haskell
class Monad m where
  instance Prelude.Monad m

  instance Prelude.Applicative m where
  -- ...
```haskell

By omitting the `where` clause, we are able to specify that the methods of
the class are provided as parameters.  This allows us to "mix" multiple classes
into a template where you provide all of the different method declarations.
It also allows us to proxy the interface of a particular class, and specify the
set of classes that should be derivable from it.


Implication Constraints
=======================

This proposal does not require much modification to other parts of Haskell.
However, one part is tricky - "It'd be nice to still have the property that
instance heads can be used as constraints in polymorphic types."  This is fine,
but additional constraints in the contexts of the generated instances get a
little funky. 

In other words, if we have

```haskell
class Foo a where
  instance Num a
  instance Bar a => Baz a where -- ...
```

Then we'd like to have a way for the generated type synonym to encode:

```haskell
type Foo a = (Num a, Bar a => Baz a)
```

I'm not much of a type theorist (though I like reading about it!), so I'm not
sure how drastic the changes to the type system would need to be.  Implication
constraints are already in core, but are not used for this purpose.   This
feature <a href="http://hackage.haskell.org/trac/ghc/ticket/5927">has been</a>
requested for other purposes, but apparently would require a rather major
overhaul.


Strengthening Constraints
=========================

Something that the [Class Alias Proposal](http://repetae.net/recent/out/classalias.html)
neglects is the ability to specify instance-specific constraints on the results.
For example, various classes characterizing the properties of Lattices are
provided, and aliases which operate on them are also provided.  However, if we
want to be able to define an instance of `BoundedLattice (a, b)`, then we need
to have the context `(BoundedLattice a, BoundedLattice b)`.  The problem with
this is that many of the instances do not require parts of this constraint.

An example with proper contexts on the generated lattices instances is given
in tests/lattice.  It looks like this:

```haskell
join :: (JoinSemiLattice a, JoinSemiLattice b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) `join` (x2, y2) = (x1 `join` x2, y1 `join` y2)

meet :: (MeetSemiLattice a, MeetSemiLattice b) => (a, b) -> (a, b) -> (a, b)
(x1, y1) `meet` (x2, y2) = (x1 `meet` x2, y1 `meet` y2)

bottom :: (BoundedJoinSemiLattice a, BoundedJoinSemiLattice b) => (a, b)
bottom = (bottom, bottom)

top :: (BoundedMeetSemiLattice a, BoundedMeetSemiLattice b) => (a, b)
top = (top, top)
```

In other words, the method signatures specify the additional constraints that
should be added to the generated instances.  This leads to a somewhat tricky
decision:

1. Allow the locations of parameter usage to effect the way constraints on
   methods are assigned to the generated instances.  The problem with this is
   that it might seem nonobvious that changing the definitions of methods in an
   instance template could change the contexts of generated instances.

2. Only do this sort of constraint business for methods that are inherited from
   a "whereless" instance (from the above section).  The problem with this is
   that if we later want to switch it to being a normal parameter, the
   constraints written in user implementations would be errors.

3. Require nested "whereless" instances in the instance template.  The problem
   with this is that it's cohesion w.r.t the typeclass hierarchy that the
   template actually generates.

I'm leaning towards #1.  Perhaps if there's a need for the user to specify in
the templates' instance declarations which parameters they depend on, it'll be
clearer to the user when they are changing the generated contexts.  The syntax
for this might look something like:

```haskell
deriving class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a ->       m b  -> m b
  return :: a      -> m a
  fail   :: String -> m a

  instance (?(>>=), ?(return)) D.Functor m where
    map f x = D.pure f D.<*> x

  instance (?(>>=), ?(return)) D.Applicative m where
    pure = return
    l <*> r = l >>= \f ->
              r >>= \x -> return (f x)

  instance (?(>>=), ?return, ?(>>)) => D.Monad m where
    (>>=) = (>>=)
    (>>)  = (>>)

  instance (?(>>=), ?return, ?(>>), ?fail) => D.MonadFail m where
    fail = fail
```

We could rely on superclass inheritance to omit the `(?(>>=), ?return)` in all
of the instances other than `D.Functor`, but this could be confusing.


Interaction with -XDefaultSignatures
====================================




Interaction with -XGenericDeriving
==================================

TODO: fill out these sections. Rest assured, these are very positive
interactions!

http://hackage.haskell.org/trac/ghc/ticket/5462

Interaction with -XTemplateHaskell
==================================

Template Haskell splits up compilation at splice points, so that local
declarations can be reified.  This screws up the semantics of overlap
resolution, because it needs to be done after processing the whole module.
There are a couple ways we could solve this:

* Have a representation for instance-templates-in-progress in TH.  We'll
  definitely want to be able to use TH to generate instance templates.

* Do template instantiation at each splice, but don't "commit" to them until
  the end of the module.  Other than contexts possibly changing (due to
  specialization), this could work pretty well.

* Do template instantiation at each splice, and commit to them at that time.


Interaction with -XGeneralizedNewtypeDeriving
=============================================


Implied Extensions
==================

Instance Templates rely on ConstraintKinds for being able to generate synonyms
for constraints.  Therefore, it makes sense to have the extension imply
`-XConstraintKinds`.  It might also makes sense to imply `-XScopedTypeVariables`
because type variables are scoped to multiple levels (though, they are levels
of declaration, rather than making it into sub-expressions / clauses).

One question is whether `-XMultiParamTypeClasses` should be necessary to invoke
a multi-parameter instance template, even if multi-parameter instances are not
generated.  I think that it should be required, because in the .

A trickier question is whether the extensions that have to do with instances
should need to be enabled when they are utilized by `-XInstanceTemplates`..
This would be a very annoying cohesion between the usage and definition of the
templates.  Certainly extensions that have to do with the interactions between
instances should be required, when necessary (-XOverlappingInstances, 
-XUndecidableInstances, -XIncoherentInstances).


Syntax Ideas / Alternatives
===========================

`deriving class` was chosen because it uses pre-existing keywords, and gives a
pretty good idea of the meaning of the declaration.  However, it does not make
much sense when contrasted with the meaning of `deriving instance`, used for 
`-XStandaloneDeriving`.

This could be made more consistent by throwing warnings whenever instance
templates are invoked through regular instance syntax, encouraging the
pre-pending of `deriving`.  Then, `deriving instance` could be used for both
standalone deriving and instance templates.
