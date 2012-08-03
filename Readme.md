Instance Templates
==================

Hardly any discussion of type class hierarchy goes without the common gripe
that Applicative ought to be the superclass of Monad.  However, restructuring
this hierarchy would break a great deal of code, particularly if we removed
redundant methods such as "pure".  This recently happened when merely removing
the Eq/Show constraints from Num.

This Haskell extension proposal provides a simple solution for reducing the
need for boilerplate class instances.  It is very similar to the
[Default Superclass Instances]
(http://hackage.haskell.org/trac/ghc/wiki/DefaultSuperclassInstances)
proposal, except that it introduces a new variety of declaration, instead of
adding this capability directly to classes.  It is also related
[Class Alias](http://repetae.net/recent/out/classalias.html) proposal, except
that it fixes some issues of this proposal while combining the power of both.

This repository contains some other documentation about this extension, as
well as a work-in-progress TH prototype of the feature.  Other than the issues
with TH and ConstraintKinds, the prototype does nearly everything that the
actual feature would need to, sans a few fairly straightforward conveniences.
Since this was possible in less than 400 lines of code (sans comments), I'm
guessing / hoping that the that the changes to GHC would be correspondingly 
conservative.  Many of these were helpers or reimplementing functionality that
would be available within GHC, for example I have a version of Haskell's
instance overlap detection (probably incorrectly, at that).


Introduction
----------

Typeclasses are naturally a source of API brittleness - doing almost anything
to them but adding a method that has a reasonable default will cause a breakage
of all the code that depends on your library.  While superclass constraints have
the benefits of providing algebraic properties and default method
implementations, they are also very unwieldy once it becomes clear that the
superclass constraints weren't gotten right the first time.

The default superclass instances proposal gives the following example:

```haskell
class Functor f => Applicative f where
  return :: x -> f x
  (<*>) :: f (s -> t) -> f s -> f t

  (>>) :: f s -> f t -> f t
  fs >> ft = return (flip const) <*> fs <*> ft

  instance Functor f where
    fmap = (<*>) . return

-- Results in the instantiation of both Functor and Applicative.
instance Applicative ZipList where
  -- ...
```

With this extension, you could instead do:

```haskell
class ApplicativeClass f where
  return :: x -> f x
  (<*>) :: f (s -> t) -> f s -> f t

  (>>) :: f s -> f t -> f t
  fs >> ft = return (flip const) <*> fs <*> ft

deriving class Applicative f where
  -- Uses the methods of "ApplicativeClass" as parameters (explained later)
  instance ApplicativeClass f

  instance Functor f where
    fmap = (<*>) . return

-- Results in the instantiation of both Functor and Applicative.
deriving instance Applicative ZipList where
  -- ... provide parameters (which implement ApplicativeClass) ...
```

The difference is that there are now two declarations - one which provides the
actual type-class - and one which gives a name for a particular strategy for
defining a set of instances.  For this variety of examples, the extension is
more verbose, but this is not an innate limitation - and trivial sugar can be
given to regain the old syntax.

By having different names for different patterns of instantiation, we can
later realize that there is a better way of specifying a set of instances, and
replace our current declaration with a reference to that, without changing our
API.  In order to do this with superclass instance defaults, you would need to
instantiate a different typeclass that has the desired defaults.  But now the
exported API is different - client code could have written constraints relying
on the class that defined the defaults.


How They Work
-------------

Another way to do the example above would be to specify an instance template
that can define an instance for `Functor` if there's already an instance of
`Applicative`:

```haskell
deriving class ApplicativeClass f => ApplicativeFunctor f where
  instance Functor f where
    fmap = (<*>) . return

instance ApplicativeClass ZipList where
  -- ...

deriving instance ApplicativeFunctor ZipList
```

The trivial, in-lining desugaring of this declaration looks like:

```haskell
instance ApplicativeClass f => Functor f where
  fmap = (<*>) . return
```

If we also want to be able to get a Functor from a Monad instance, we can do:

```haskell
deriving class Monad m => MonadicFunctor m where
  instance Functor m where
    fmap f x = x >>= return . f
```

It desugars similarly.  With Superclass Default Instances we would have had a
few ugly choices on our hands:

* Add default instances to `Monad` and `Applicative`, and require "hiding"
  declarations whenever the user defines both.

* Define separate classes akin to these templates, which unfortunately exposes
  (in the API) which was used to generate the `Functor`.

Note that this variety of instance templates allows for the generation of
instances for classes that you do not control, giving us the ability to adapt
one class hierarchy to another.  However, this doesn't "come for free" - and
requires that you explicitly request it.


Parameters
----------

The substitution of the instances generated by an instance template occurs in a
referentially transparent fashion - it doesn't matter what the template is
called, all that matters is how the parameters are substituted.

What parameters?  There are two varieties: type parameters, and value
parameters.  The type parameters look the same as the type parameters of a
typeclass, and the value parameters look the same as method signatures /
defaults.

How are they substituted?  Well, in the way you'd expect, but we need to
determine how to resolve scope.  I think that the proper decision here is to
have the parameters shadow the scope as if they were provided via a where
clause (at least for the methods).

Here's an example of how substitution works:

```haskell
class Class a where
  method :: a

deriving Template a where
  parameter :: a
  instance Class a where
    method = parameter

deriving instance Template Int where
  parameter = 10
```

This template invocation could then be desugared to:

```haskell
instance Class Int where
  method = parameter
    where
      parameter = 10
```

(direct substitution would also work, but this de-sugaring makes the naming
rules clear)

Note that despite these looking like instance declarations, "parameter" will
not be exported as a method (this would require there to be a typeclass to
attach it to).  This is nice because it allows you to hide things that are
merely configuration for the generated instances, which don't make sense to
export for general usage on the datatypes.

One of the main applications of these instance templates is to export a
template that allows for a specification that looks like it targets an old
type-class, when it really targets one or more typeclasses in the new
hierarchy.  This gets around the issue described earlier of needing to
"explicitly request" the derivation of instances.  There are [examples]
(https://github.com/mgsloan/instance-templates/blob/master/tests/) of doing
this, as well as a description of how this allows Haskell source code to
express most trivial [API deltas]
(https://github.com/mgsloan/instance-templates/blob/master/doc/Deltas.md).


Mixin Sugar
-----------

In the introduction, this was used with some handwaving to provide an
equivalent definition of the `Applicative` from superclass defaults.  The
current plan for syntactic sugar is to have `instance Foo a`, without a
`where` mean that the methods should "mixed in".  In other words, they are
used as parameters to the template, but are also provided to the instance.  By
having this sugar, much of the convenience of the "Class Alias" proposal is
achieved.

For example, we can export a template which allows you to define a whole
suite of operations:

```haskell
deriving class Num a where
  instance Addable a
  instance Multiplicable a
-- etc ... (see tests folder for full code)
```

Or, for even more concision, this sugar could work with type constraint
synonyms / tuples of constraints:

```haskell
deriving class Num a where
  instance ( Addable a, Multiplicable a, ... )
```


Relationship to Superclass Default Instances
--------------------------------------------

As mentioned in that page, default superclass instances have been a "matter of
consternation" for some time, as no approach to the problem has been
satisfying enough to be implemented.   The functionality of Instance Templates
is very similar to Superclass Default Instances, but there are a number of
important distinctions that give it much more appealing properties:

The main distinction, as described earlier, is that this proposal gives a way
to name different patterns of instantiating multiple instances, even if these
patterns instantiate the same set of instances.  This is simply not possible
with Superclass Default Instances.  As a result of not exporting

One ugliness of superclass default instances is that it tries to wedge in the
features of the class alias proposal:

* The method definitions in `...defs...` are distributed to the appropriate
  instance declaration, according to which class the method belongs to.

* Any methods that are not specified explicitly are "filled in" from the
  default definition given in the default superclass instance. (If there is no
  default definition, then a warning is produced, and a definition that calls
  `error` is used instead.)

This is very unprincipled - for one thing, having multiple superclass
constraints use the same typeclass is impossible.  Even more dire is the idea
that you would want to mix your own function definitions into the default
instances.  The conceptual difference between "default" vs "generated"
instance is really where these two proposals diverge.

Another difference is that the implicit suppression, to be described, can
reasonably handle the interaction between multiple templates.


Module Envy
-----------

Those familiar with ML's module system might recognize the likeness of this
idea to module functors.  The analogy is more than superficial - one potential
desugaring for instance templates would be fairly equivalent to the translation
of Haskell modules to ML given by [Stefan Wehr]
(http://www.stefanwehr.de/publications/Wehr_ML_modules_and_Haskell_type_classes.pdf).
I am not experienced enough with ML to know whether this translation and
extension represent the full gamut of module functor capabilities.  It
certainly does not do module functors of higher order (I believe that there is
a little-used ML extension for this).

One inspiration we can take from ML functors is having helper temporary
values, in terms of the parameters, for use in multiple methods.  For example,
we could use instance templates to generate a Num instance whenever we have an
instance of Num for some type that we have a bijection with:


```haskell
data Bij a b = Bij
  { fwd :: (a -> b)
  , bwd :: (b -> a)
  }

deriving class Num a => BijNum a b where
  bij :: Bij a b

  instance Num b where
    x + y = fwd bij (bwd bij x + bwd bij y)
    -- ...
```

But this pattern would get old fast, especially for higher arities.  What if
we could do this instead:

```haskell
deriving class Num a => BijNum a b where
  bij :: Bij a b

  let bin f x y = fwd bij $ f (bwd bij x) (bwd bij y)

  instance Num b where
    (+) = bin (+)
    -- ...
```

Much better!  When instantiated, these could de-sugar to unexported, uniquely
named types.

We can get the equivalent of this for types by using type-equalities in the
context of the template (assuming that something like `-xScopedTypeVariables`
is enabled).


Usage in Constraints
--------------------

It'd be nice to still have the property that instance heads can be used as
constraints in polymorphic types.  As luck would have it, the ConstraintKinds
extension provides us just what we need - constraint synonyms!

```haskell
type Monad m = (Functor m, Applicative m, Monadic m, MonadFail m)
```

On the topic of these more recent type-system enhancements, while I have not
yet implemented it in the TH prototype, it seems like having type and data
family declarations will be relatively straightforward.  This is true of both
the head of the `deriving class` (which resemble class syntax), and the
generated instances (which use instance syntax).

This is because the parameters are directly substituted into the generated
instance.  So, a type family instance declaration will be substituted into all
usages of that type family in the generated instances.  It seems like data
family declarations could be more problematic because it seems like their
usage should usually be linear, as it makes less sense to define a data-type
multiple times (the names of the constructors would conflict).


More Stuff
----------

There's a bit more to say about this idea!

* [A few examples of how it's useful]
  (https://github.com/mgsloan/instance-templates/blob/master/doc/Examples.md)

* [How the feature might be used with old code]
  (https://github.com/mgsloan/instance-templates/blob/master/doc/OldCode.md).

* [Other details, and interactions with other extensions]
  (https://github.com/mgsloan/instance-templates/blob/master/doc/Details.md)

* [We gain additional ability to represent API differences]
  (https://github.com/mgsloan/instance-templates/blob/master/doc/Deltas.md)

* [Some more ideas and notes]
  (https://github.com/mgsloan/instance-templates/blob/master/doc/Extras.md)


Thanks to all the people who gave me feedback and encouragement on this
proposal! Constructive criticism is welcome!
