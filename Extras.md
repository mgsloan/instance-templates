First Class Modules
===================

[The first class modules paper](http://research.microsoft.com/en-
us/um/people/simonpj/Papers/first-class-modules/first_class_modules.pdf)
provides one potential solution for unifying record types and the module
system of Haskell.  I do not understand how typeclasses work in the proposed
system, but it would make sense that they would have a representation very
related to the one used for modules / records, since they contain much the
same thing.

Module functors in ML allow definitions and types to be similarly parameterized
on input parameters.

[This paper](http://www.stefanwehr.de/publications/Wehr_ML_modules_and_Haskell_type_classes.pdf)
gives a translation between first class modules and typeclasses.  I wish I'd
realized this earlier in the design process!  It may suggest a superior de-
sugaring (instead of having TH that writes TH-utilizing code).  The current
de-sugaring has some similarities - it needs to use a dummy datatype to give
identity to the functor.  The main difference is that the parameters are
provided via superclass constraint (avoiding repetition), whereas here we
provide them via where declarations.


Interaction with -XDefaultSignatures
====================================

Interaction with -XGenericDeriving
==================================

TODO: fill out these sections. Rest assured, these are very positive
interactions!

http://hackage.haskell.org/trac/ghc/ticket/5462

Interaction with -XTemplateHaskell
==================================

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

Scope-Restricted Weak Typing
============================

This is not at all a crucial point of my proposal, however, it is the kind of
thinking about instances that falls out of having a capability like this.

It's very tempting to give your language the 'intelligence' to implicitly apply
a function in order to resolve what would otherwise be a type error.  Common
examples of weak typing found in other languages are `Int -> Float`,
`Bool -> Int`, `Int -> String`, `String -> Int` and even `Float -> Int`.

The main example of this is anything that's newtype-ish - where we have
`a -> b` and `b -> a`.  An instance template could take these two functions and
yield the instance that would have been generated if you could inject a custom
constructor / destructor into GeneralizedNewtypeDeriving.  For example, Num:

```haskell
deriving Num b => BijNum a b where
  f :: a -> b
  g :: b -> a

  instance Num a where
    x + y         = g (f x + f y)
    x * y         = g (f x * f y)
    x - y         = g (f x - f y)
    negate      x = g (negate      (f x))
    abs         x = g (abs         (f x))
    signum      x = g (signum      (f x))
    fromInteger x = g (fromInteger (f x))
```

We can automatically generate this definition by processing the type signatures
of the methods in the class.  Parameters that are `a` should have `f` applied
to them, and results of type `a` should have `g` applied to them.

By extending this rewriting to more cases, we can get more sophisticated
templates.  For example, it could be specified that a result of type `f a`
should have `fmap g` applied to it.  By giving a notation for specifying this
rewriting, we are conceptually introducing the convenience of weak-typing, but
properly scoped to a known set of definitions / value junctures.


Relationship to -XGeneralizedNewtypeDeriving
============================================




Relationship to Other Proposals
===============================

(This section is incomplete)

The question might be asked "Why wasn't this thought of before?".  The answer
is "I'm not sure", but I think that it likely has been thought about before,
just disregarded due to some preconceived notions of a desirable mechanism.
By focusing on extending the typeclass system directly, we end up with a very
complicated set of trade-offs, that are tough to navigate.  Instead, this
approach adds an entirely new type of declaration, and adds a new meaning for
instance declarations.  Rather than "changing" the meaning of instances, we
are allowing them to be applied to something they couldn't before - constraint
types.


Here's a design goal from the superclass instances write-up. It's given
as the reason that an "Opt-In" scheme is undesirable, without much further
explanation.
  
> Design goal 1: a class C can be re-factored into a class C with a
> superclass, without disturbing any clients.

I think that this is still quite possible with an Opt-In scheme, we
just need to make instance declarations potentially mean something
quite different than before (when applied to constraint synonyms).


http://www.haskell.org/haskellwiki/Superclass_defaults

This proposal, and mine, play quite nicely with constraint synonyms -
instance templates can have a compound class constraint in the type
argument.

Where this proposal falls flat is that it still relies on the
defaulting system as its mechanism, leading to strange things:

> If both Class1 and Class2 have a default implementation, and Class1
> is a (indirect) superclass of Class2, then the default from Class1
> is ignored.

Also, by trying to wedge superclass defaults into the existing syntax,
we end up with a ton of funky restrictions:

> Subject to the constraint that:
> * No class appears more than once in the list.
> * The arguments to each class are the same.
> * ... the superclass relation gives a connected acyclic graph with a
    single source, the most specific class in the hierarchy.

This is also a weakness in the Strathyclyde Haskell Enhancement's
implementation of default superclass instances.



Musings on Default Superclass Instances
=======================================

Even in the presence of instance templates, default superclass instances would
be a very useful feature.  With instance templates, we can say "Oops, we
messed up!", and modify the library that defines a particular class without
disturbing clients - an excellent step forward. However, if we want to be able
to have clients of that class be able to use our library (which uses a
different typeclass), with no further work or modification of that library,
then we're in trouble.

At once, we need to worry about the case in which the user wants to override
this instance with a more efficient implementation.  In order to support this,
we need to make these superclass instances a very different sort of instance,
that can be "overrided" (this is the "default" part of "superclass defaults").

If we still want to support the following features of Haskell typeclasses:

* Orphan instances

* Consistency regarding which instances are used for a particular set of
  monomorphic types

Then we need to have global knowledge of our program when deciding when to
override default superclass instances.  This is fine, because we can do it at
link time, however it violates the currently held expectation that we know
locally how an instance is implemented.  Such is the price of convenience!

These are really quite different beasts than standard instances, and so I
would suggest that they have distinct syntax.  My proposal for instance
templates suggests that the following might be reasonable:

```haskell
deriving class Monad a => where
  instance Functor a where
    fmap f = (>>= return . f)
```

In other words, default superclass instances are just anonymous instance
templates!

The next problem is that these is there's ambiguity in the case that these
anonymous instance templates conflict.  I have an intuition that the scheme
used to resolve this with the named instance templates might be brought to
bear on this problem.