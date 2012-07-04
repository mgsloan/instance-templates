Instance Templates
==================

This goes a little bit beyond "re-skinning" Haskell, however is fairly
equivalent to introducing TH functions of the variety that are already pretty
popular: deriving class instances.

The purpose of these declarations is to provide a mechanism for expressing how
a set of interfaces can be expressed in terms of some other interface.  This
allows us to restructure class hierarchies without breaking client code, and
reduce code duplication found in boilerplate instances.

```haskell
deriving class PreOrder a where
  (<=) :: a -> a -> Bool

  instance Eq a where
    x == y = (x <= y) && (y <= x)

  instance Ord a where
    (<=) = (<=)
```

The immediately nested `(<=)` signature declares a "parameter" of the
"instance template".  These are called parameters because they are only used to specialize the generated instances, and don't get exported.

The `(<=)` nested inside `instance Ord` is not ambiguous with the outer one,
because the template parameters shadow the methods defined within (method
definitions can reference each other, though).

In order to use the "deriving class PreOrder" declaration, we write code that
looks like this:

```haskell
instance PreOrder Bool where
  False <= _ = True
  True  <= x = x
```

The methods defined in the instance are used as the parameters to the instance
template, expanding into:

```haskell
instance Eq a where
  x == y = (x <= y) && (y <= x)
   where
    False <= _ = True
    True  <= x = x

instance Ord a where
  (<=) = (<=)
   where
    False <= _ = True
    True  <= x = x
```

This is the trivial, definitional desugaring - the compiler could certainly do something more clever.

It'd be nice to still have the property that instance heads can be used as
constraints in polymorphic types.  As such, I think it's reasonable for the
above definition to implicitly create the following constraint-kind synonym:

```haskell
type PreOrder a = (Eq a, Ord a)
```

Resolving Overlap
-----------------

We can now make instances mean something different, and generate more class instances than before.  This has a few implications:

* The generated instances might conflict with those already defined in the
  module.  In this case, I think that this is a very reasonable mechanism for
  "opt-ing out" of a particular generated instance.  However, it could be
  unexpected, so a pragma-suppressible warning should be generated for this
  sort of overlap.

* The instances a particular module exports is now dependent on the way the
  classes are defined in its imports. This is not as much of an issue as it
  sounds - an instance of `Monad` will still mean we have an instance of
  `Monad` - be it a normal or compound class constraint.

  However, this is an issue when it comes to orphan instances, as identical
  library / client code could have an instance clash, given a different set of
  definitions for the library's dependencies.I think that this is acceptable,
  as orphan instances are known to be dangerous, and many instance templates
  would not have this sort of behavior.

* The generated instances might conflict with those produced by other template
  invocations.  This is something that we'd quite reasonably want to do, while
  re-working entire hierarchies.  The rest of this section is devoted to this
  issue.

For example, we could write an instance template for the old version of
`Applicative`, that generated the new, properly hierarchical versions of
`Functor` and `Applicative`.  We could then also write an instance template
for `Monad` that generated all three.

The problem with this is that code using the current hierarchy would quite
often reasonably have instances for all three, so two versions of
`Applicative` would be generated. So, the question becomes how we can tell
which definition supersedes, without hackily defining an arbitrary priority
order.

A very reasonable answer is suggested by analogy to the solution used to deal
with conflicts with regular instances - have explicit invocations of the
instance template supersede those specified inside an instance template.
For example:

```haskell
deriving class Foo a where
  foo :: a

deriving class Bar a where
  bar :: a -> a
  baz :: a

  instance Foo a where
    foo = bar baz

  instance Enumerable a where
    enumerate = baz : map bar enumerate
```

If we have:

```haskell
instance Bar Int where
  bar = (+1)
  baz = 0
```

Then the following gets generated (for clarity, the definitions that would be in
where declarations have been substituted):

```haskell
instance Foo Int where
  foo = (+1) 0

instance Enumerable Int where
  enumerate = 0 : map (+1) enumerate
```

If, instead we use both instance templates:

```haskell
instance Foo Int where
  foo = 2

instance Bar Int where
  bar = (+1)
  baz = 0
```

Then we get:

```haskell
instance Foo Int where
  foo = 2

instance Enumerable Int where
  enumerate = 0 : map (+1) enumerate
```

So, we've manged to be able to mix and match instance templates!


Why?
====

* It's simple.  We're just supplying values to a generic instance, to create a
  specific one, and these parameters are referentially transparent.

* More powerful instance derivation allows us to mitigate the impact of
  historical decisions.

  Being able to rework, say, the Numeric class hierarchy, is the main goal of
  this proposal (and those that came before):
  http://hackage.haskell.org/trac/ghc/wiki/DefaultSuperclassInstances
    
  As mentioned in that page, default superclass instances have been a "matter
  of consternation" for some time, as no approach to the problem has been
  satisfying enough to be implemented.  By forcing the decision of how to
  implement a class to be per-datatype, we avoid attempting to define
  typeclass instances which "always" can be implemented universally in terms
  of some other.

* In order to create configurable instances (see the "weak typing" section)
  with superclass instances, we need to create a new typeclass.  This is odd,
  because it doesn't make much sense to use it in any other context than
  the superclass of the instance.  I think it's preferable to be able to "hide"
  this class so that it can't be depended on.

* Avoidance of TH.

  - People have observed many things that are wrong with TH / mis-aligned with
    Haskell philosophy.
    http://stackoverflow.com/questions/10857030/whats-so-bad-about-template-haskell/

  - This feature can be implemented as a TH library, though requiring special
    delineation around usages.  However, the error messages and potential for
    analysis by tools would be impaired.  By making it a language feature, we
    can conquer even more of the useful regions of macro-expansion design space
    and reduce the necessity of TH.

  - Compared to the power and complexity of TH, this feature is very simple,
    and we can rely on the reasoning power of referential transparency.

"Problems"
==========

No potential solution to the "default superclass instances" is without its
trade-offs.  I think that the described solution is a straightforward,
understandable solution to the problem, that buys a lot of power, with
comparatively minimal issues:

* We cannot create instances that are automagically derived in the presence of
  some set of constraints.  This was the intent of the problematic "superclass-
  defaults" proposals, and is a huge can of worms.  I think that wanting this
  feature is a little over-reaching, if you want to maintain Haskell's current
  typeclass semantics.

* Ideally we'd be able to seamlessly use old code with our new typeclasses, in
  concordance with "Design goal 1" mentioned in the "Other Proposals" section.
  However, this means that this feature would need to only have a LANGUAGE
  extension option (e.g. -XInstanceTemplates) for the modules defining
  instance templates.

  Are we comfortable changing the meaning of code without additional pragma,
  in the event that the dependencies specify this pragma?  Is there any
  precedent for language extensions doing this?

  If ConstraintKinds is used as the mechanism that allows for naming the set
  of derived instances, then this would also mean that -XConstraintKinds would
  need to be implicit in users of instance templates.  This is not so
  disastrous, though - as this could be restricted to usage of constraint
  kinds, and not actual declaration of them / usage of "Constraint".

Credits
=======

The following people (and more that go unmentioned!) all gave me invaluable
encouragement, feedback, and insights in the process of refining this document
and its ideas:

* Edward Kmett (edwardk)
* Luite Stegeman (luite)
* Drew Day (tgeeky)
* William Cauchois (wcauchois)
