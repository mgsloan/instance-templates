TODO: finish this file.

My solution to this is to discourage the usage of implicit instance suppression
and have two different syntactic forms: one for explicit invocation of a
template, and one that is transitional, with kludgy instance suppression.  The
scope of this suppression is limited to the module that they're defined in,
eliminating the <a href="http://lukepalmer.wordpress.com/2009/01/25/a-world-without-orphans/">trickiness of orphans</a>.


"Problems"
----------

* Ideally we'd be able to seamlessly use old code with our new typeclasses, in
  concordance with "Design goal 1" mentioned in the "Other Proposals" section
  of Default Superclass Instances. However, this means that this feature would
  need to only have a LANGUAGE extension option (e.g. -XInstanceTemplates) for
  the modules defining instance templates.

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
