
/*
   This just provides documentation for stuff that doesn't need to be in the
   source headers themselves.  It is a ".cc" file for the sole cheesy reason
   that it triggers many different text editors into doing Nice Things when
   typing comments.  However, it is mentioned nowhere except the *cfg.in files.
   Pieces separated by '// //' lines will usually not be presented to the
   user on the same page.
*/

// // // // // // // // // // // // // // // // // // // // // // // //
/** @namespace std
 *  @brief Everything defined by the ISO C++ Standard is within namespace
 *  std.
*/

// // // // // // // // // // // // // // // // // // // // // // // //
/** @addtogroup SGIextensions STL extensions from SGI
Because libstdc++-v3 based its implementation of the STL subsections of
the library on the SGI 3.3 implementation, we inherited their extensions
as well.

They are additionally documented in the
<a href="http://gcc.gnu.org/onlinedocs/libstdc++/documentation.html">
online documentation</a>, a copy of which is also shipped with the
library source code (in .../docs/html/documentation.html).  You can also
read the documentation <a href="http://www.sgi.com/tech/stl/">on SGI's
site</a>, which is still running even though the code is not maintained.

<strong>NB</strong> that the following notes are pulled from various
comments all over the place, so they may seem stilted.
<hr>
*/

// // // // // // // // // // // // // // // // // // // // // // // //
// This is standalone because, unlike the functor introduction, there is no
// single header file which serves as a base "all containers must include
// this header".  We do some quoting of 14882 here.
/** @addtogroup Containers Containers
Containers are collections of objects.

A container may hold any type which meets certain requirements, but the type
of contained object is chosen at compile time, and all objects in a given
container must be of the same type.  (Polymorphism is possible by declaring a
container of pointers to a base class and then populating it with pointers to
instances of derived classes.  Variant value types such as the @c any class
from <a href="http://www.boost.org/">Boost</a> can also be used.

All contained types must be @c Assignable and @c CopyConstructible.
Specific containers may place additional requirements on the types of
their contained objects.

Containers manage memory allocation and deallocation themselves when
storing your objects.  The objects are destroyed when the container is
itself destroyed.  Note that if you are storing pointers in a container,
@c delete is @e not automatically called on the pointers before destroying them.

All containers must meet certain requirements.  They would be listed here
except I'm not certain how much of 14882 can be reproduced without a
copyright violation.  Reproducing Tables 65 through 69 is a lot of typing...

The standard containers are further refined into
@link Sequences Sequences@endlink and
@link Assoc_containers Associative Containers@endlink.
*/

/** @addtogroup Sequences Sequences
Sequences arrange a collection of objects into a strictly linear order.

The differences between sequences are usually due to one or both of the
following:
  - memory management
  - algorithmic complexity

As an example of the first case, @c vector is required to use a contiguous
memory layout, while other sequences such as @c deque are not.

The prime reason for choosing one sequence over another should be based on
the second category of differences, algorithmic complexity.  For example, if
you need to perform many inserts and removals from the middle of a sequence,
@c list would be ideal.  But if you need to perform constant-time access to
random elements of the sequence, then @c list should not be used.
*/

/** @addtogroup Assoc_containers Associative Containers
Associative containers allow fast retrieval of data based on keys.

Each container type is parameterized on a @c Key type, and an ordering
relation used to sort the elements of the container.
*/

// // // // // // // // // // // // // // // // // // // // // // // //

