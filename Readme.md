Instance Templates
==================

Hardly any discussion of type class hierarchy goes without the common gripe
that Applicative ought to be the superclass of Monad.  However, restructuring
this hierarchy would break a great deal of code, particularly if we removed
redundant methods such as "pure".

This Haskell extension proposal provides a simple solution for refactoring
type-class hierarchies while maintaining backwards compatibility.  However, it
can also do more than this, allowing boilerplate declarations to be generated
in terms of some common pattern of usage.  Functions in Haskell enjoy the
reasoning and abstraction benefits of referential transparency.  This
extension gives us this same power, but for instance declarations, allowing
for the abstraction of common instantiation patterns.

This repository contains some other documentation about this extension, as well
as a work-in-progress TH prototype of the feature.  The file "Rewriter.hs" uses
the haskell-src-exts package to rewrite Haskell code to allow regular classes
and instances to use this framework.

This proposal shares some goals with [Default Superclass Instances](
http://hackage.haskell.org/trac/ghc/wiki/DefaultSuperclassInstances),
but achieves these goals in a fashion that's easier to understand and 
implement, of course making a different set of trade-offs.  We sacrifice
the ability to restructure hierarchies without touching the actual code,
in exchange for being capable of much more.


Description
-----------

```haskell
deriving class PreOrder a where

  -- A list of parameters to the instance template.  These get substituted into
  -- the instances below.  They're not exported - and this symbol will only
  -- exist externally if one of the instances re-exports it.

  leq :: a -> a -> Bool

  -- The instances generated by the instance template.

  instance Eq a where
    x == y = leq x y && leq y x

  instance Ord a where
    (<=) = leq
```

In order to use this `deriving class PreOrder` declaration, we write code that
looks like this:

```haskell
instance PreOrder Bool where
  leq False _ = True
  leq True x = x
```

The methods defined in the instance are used as the parameters to the instance
template, expanding into:

```haskell
instance Eq a where
  x == y = leq x y && leq y x
   where
    leq False _ = True
    leq True x = x

instance Ord a where
  (<=) = leq
   where
    leq False _ = True
    leq True x = x
```

This is the trivial, definitional desugaring - the compiler could certainly
do something more clever.

It'd be nice to still have the property that instance heads can be used as
constraints in polymorphic types.  As a result, it's reasonable for the
above definition to implicitly create the following constraint-kind synonym:

```haskell
type PreOrder a = (Eq a, Ord a)
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


Resolving Overlap
-----------------

We can now make instance declarations generate more class instances than
before, which leads to some things to consider:

* The generated instances might conflict with those already defined in the
  module.  This is a good primary mechanism for "opt-ing out" of a particular
  generated instance.  Since this is adding some "spooky action at distance",
  it's reasonable to generate WARNINGs in order to notify the user that
  something unexpected might happen.

  "Hiding" declarations would be a good way to opt out.  These hiding
  declarations could refer to constraint synonyms (which might have been
  generated by other instance templates), in order to succinctly suppress
  a bunch of the generated instances.

  This particular design decision has been much discussed much within the
  context of Superclass Default Instances.  Within that proposal, this is
  called "Option 2", and seems to be the most popular / the consensus.

* The instances a particular module exports is now dependent on the way the
  classes are defined in its imports. This is not as much of an issue as it
  sounds - an instance of `Monad` will still mean we have an instance of
  `Monad` - be it a normal or compound class constraint.

  However, this is an issue when it comes to orphan instances, as identical
  library + client code could have an instance clash, given a different set of
  definitions for the library's dependencies.  I think that this is acceptable,
  as orphan instances are known to be dangerous, and many instance templates
  would not have this sort of behavior.

* The generated instances might conflict with those produced by other template
  invocations.  This is something that we'd quite reasonably want to do, while
  re-working entire hierarchies.  The rest of this section is devoted to this
  issue.

  For example, we could write an instance template for the old version of
  `Applicative`, that generated the new versions of `Functor` and
  `Applicative`.  We could then also write an instance template for `Monad` that
  generated all three.

  The problem with this is that code using the current hierarchy would quite
  often reasonably have instances for all three, so two versions of
  `Applicative` would be generated. So, the question becomes how we can tell
  which definition supersedes, without hackily defining an arbitrary priority
  order.

  A reasonable way to do this is by allowing an instance template that
  generates a subset of another to have priority.  "Subset" here means that all
  of the instances generated by one template overlap with some instance in
  another template (causing those to be suppressed).


Why?
----

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

* Avoidance of Template Haskell.

  - The TH implementation of this library generates TH code.  In other words,
    it's making it convenient to write code that was already quite possible to
    write before.  However, the error messages and potential for analysis by
    tools are impaired.  By making it a language feature, we can conquer a good
    deal of the macro-expansion design space for typeclasses.


  - People have observed many things that are wrong with TH / mis-aligned with
    Haskell philosophy.  I find it to be an extremely useful and invaluable
    tool despite, but many of these points are valid.

    http://stackoverflow.com/questions/10857030/whats-so-bad-about-template-haskell/


  - Compared to the power and complexity of TH, this feature is very simple,
    and we can rely on the reasoning power of referential transparency.


Why not Superclass Default Instances?
=====================================

The #1 difference between this proposal and superclass default instances is
that instance templates are not typeclasses.  The weaknesses of the Default Superclass Instances can be exposed in the
<a href="http://hackage.haskell.org/trac/ghc/wiki/DefaultSuperclassInstances#Instancedeclarations">
instance declarations</a> section.  There, it says:

* An instance declaration

  ```haskell
  instance Q => C ty where ...defs...
  ```

  for class C generates an extra instance declaration

  ```haskell
    instance Q => Si ty where ....
  ```

  for each intrinsic superclass Si of C

* The method definitions in `...defs...` are distributed to the appropriate
  instance declaration, according to which class the method belongs to.

* Any methods that are not specified explicitly are "filled in" from the
  default definition given in the default superclass instance. (If there is no
  default definition, then a warning is produced, and a definition that calls
  `error` is used instead.)


The first bullet is identical to the behavior that I specify, except instead
of there being the concept of an "extra" instance, there's merely the concept
of the set of instances being generated.

The other two bullets are the real problems.  If you "distribute" methods
among their implementations, then we cannot support multiple superclasses of
the same type.  Allowing for the partial overriding of these default instances
could lead to unexpected results, and the author of the default cannot restrict
this.

This is a result of trying to jam this stuff into typeclasses instead of
adding a new construct.  By requiring the instances to be defined within a
typeclass, this means that we can write subclasses and use this new typeclass
in constraints.  I argue that this is a problem, and breaks the ability to do
proper substitution - we've given up referential transparency.  In essence,
this is the difference between having a `type` and a `newtype`.  Not being
able to declare referentially transparent instances is like having `f = g`,
yet being able to distinguish `f` and `g`.

I think that there'd actually be good reason to support superclass default
instances in terms of instance templates.  The reason would be to reduce the
potential for a proliferation of names due to needing one for the template and
one for the instance template.  Therefore, there'd be a straightforward de-
sugaring from:

```haskell
class JoinSemiLattice a where
  join :: a -> a -> a

  instance Eq a => PartialOrder a where
    cmp x y
      | x == j = Just GT
      | x == y = Just EQ
      | j == y = Just LT
      | _ = Nothing
     where
      j = join x y
```

To:
```haskell
class PartialOrder a => JoinSemiLatticeClass a where
  join :: a -> a -> a

deriving class JoinSemiLattice where
  instance JoinSemiLatticeClass a

  instance Eq a => PartialOrder a where
    cmp x y
      | j == x = Just GT
      | j == y = Just LT
      | x == y = Just EQ
      | _ = Nothing
     where
      j = join x y
```

This is quite similar in feel to the `-XDefaultSignatures` extension.


"Problems"
----------

No potential solution to the "default superclass instances" is without its
trade-offs.  I think that the described solution is a straightforward,
understandable solution to the problem, that buys a lot of power, with
comparatively minimal issues:

* We cannot create instances that are automagically derived in the presence of
  some set of constraints.  This would allow you to adapt a typeclass hierarchy
  without breaking other code or having them depend on / know about your code.
  Not even the superclass instance defaults proposal would solve this.
  Handling this would require major 

* Ideally we'd be able to seamlessly use old code with our new typeclasses, in
  concordance with "Design goal 1" mentioned in the "Other Proposals" section.
  However, this means that this feature would need to only have a LANGUAGE
  extension option (e.g. -XInstanceTemplates) for the modules defining
  instance templates.

  Are we comfortable changing the meaning of code without additional pragma,
  in the event that the dependencies specify this pragma?  Is there any
  precedent for language extensions doing this?

  [Niklas Broberg's take on this, in the context of superclass default instances]
  (http://www.mail-archive.com/glasgow-haskell-users@haskell.org/msg20351.html)

  If ConstraintKinds is used as the mechanism that allows for naming the set
  of derived instances, then this would also mean that -XConstraintKinds would
  need to be implicit in users of instance templates.  This is not so
  disastrous, though - as this could be restricted to usage of constraint
  kinds, and not actual declaration of them / usage of "Constraint".

* One bit of ugliness is that now we can't move an instance declaration that
  is overlapped by an instance template into another file, without having a
  declaration of the form `hiding instance ...` that suppresses the generated
  instance.

    - One alternative is to have instance templates be a part of the exported
      signature, and then later do whole-program instantiation of these
      templates.  This is the same sort of resolution that would need to take
      place for superclass instance defaults to work out while preserving this
      mobility-of-instances property.

    - The difference in behavior found when moving an instance out of a module
      can actually be beneficial - we can use an instance template which
      overlaps with those defined in the module, even if its definition
      doesn't know about them!  All of the generated instances that overlap
      with the ones that already exist will be suppressed.

More Stuff
----------

There's a bit more to say about this idea!

* [A few examples of how it's useful]
  (https://github.com/mgsloan/instance-templates/blob/master/Examples.md)

* [Other details, and interactions with other extensions]
  (https://github.com/mgsloan/instance-templates/blob/master/Details.md)

* [We gain additional ability to represent API differences]
  (https://github.com/mgsloan/instance-templates/blob/master/Deltas.md)

* [Some more ideas and notes]
  (https://github.com/mgsloan/instance-templates/blob/master/Extras.md)


Thanks to all the people who gave me feedback and encouragement on this
proposal! Constructive criticism is welcome!
