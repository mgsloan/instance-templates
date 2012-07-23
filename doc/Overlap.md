In-Depth on Overlap
===================

Alright, so this overlap resolution stuff is tricky.  Let's list the benefits
of each method.

* Explicit "hiding" declarations.

    - This is nice because it's explicit, and probably preferable for self-
      documentation.  It's also not something that is ruled out by the adoption
      of one of these other behaviors.  The explicit declaration can be the
      mechanism for suppressing the WARNINGs that would otherwise be generated
      by the other strategies.

    - This isn't so great because it means that restructuring inheritance
      hierarchies isn't possible.

* Explicit chaining of instance templates.

    - This is currently what the proposal specifies.  The idea is that the
      subsumption should be explicit in the template invocation hierarchy.
      This is nice, because it's straightforward.

    - This isn't ideal, because it means that in order to write a convenient

* Resolution by subset overlap.

    - The idea here is to take the set of concrete instances, and see if

    - One downside is that this could actually be fairly asymptotically
      complex.  This shouldn't be a problem with anything but generated code,
      but I think it would break down something like this:

      The 

    - 