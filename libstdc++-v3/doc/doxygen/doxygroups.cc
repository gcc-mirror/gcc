/*
   Copyright (C) 2001-2016 Free Software Foundation, Inc.
   See license.html for license.

   This just provides documentation for stuff that doesn't need to be in the
   source headers themselves.  It is a ".cc" file for the sole cheesy reason
   that it triggers many different text editors into doing Nice Things when
   typing comments.  However, it is mentioned nowhere except the *cfg.in files.

   Some actual code (declarations) is exposed here, but no compiler ever
   sees it.  The decls must be visible to doxygen, and sometimes their real
   declarations are not visible, or not visible in a way we want.

   Pieces separated by '// //' lines will usually not be presented to the
   user on the same page.
*/

// // // // // // // // // // // // // // // // // // // // // // // //
/** @namespace std
 *  @brief ISO C++ entities toplevel namespace is std.
*/
/** @namespace std::__detail
 *  @brief Implementation details not part of the namespace std interface.
*/
/** @namespace std::tr1
 *  @brief ISO C++ TR1 entities toplevel namespace is std::tr1.
*/
/** @namespace std::tr1::__detail
 *  @brief Implementation details not part of the namespace std::tr1 interface.
*/
/** @namespace std::tr2
 *  @brief ISO C++ TR2 entities toplevel namespace is std::tr2.
*/
/** @namespace std::tr2::__detail
 *  @brief Implementation details not part of the namespace std::tr2 interface.
*/
/** @namespace __gnu_cxx
 *  @brief GNU extensions for public use.
*/
/** @namespace __gnu_cxx::__detail
 *  @brief Implementation details not part of the namespace __gnu_cxx 
 *  interface.
*/
/** @namespace __gnu_internal
 *  @brief GNU implemenation details, not for public use or
 *  export. Used only when anonymous namespaces cannot be substituted.
*/
// // // // // // // // // // // // // // // // // // // // // // // //

/**
 * @defgroup extensions Extensions
 *
 * Components generally useful that are not part of any standard.
 */

/** @defgroup SGIextensions SGI
 * @ingroup extensions
Because libstdc++ based its implementation of the STL subsections of
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

/** @defgroup containers Containers
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

All containers must meet certain requirements, summarized in
<a href="tables.html">tables</a>.

The standard containers are further refined into
@link sequences Sequences@endlink and
@link associative_containers Associative Containers@endlink.
@link unordered_associative_containers Unordered Associative Containers@endlink.
*/

/** @defgroup sequences Sequences
 * @ingroup containers
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

All sequences must meet certain requirements, summarized in
<a href="tables.html">tables</a>.
*/

/** @defgroup associative_containers Associative
 * @ingroup containers
Associative containers allow fast retrieval of data based on keys.

Each container type is parameterized on a @c Key type, and an ordering
relation used to sort the elements of the container.

All associative containers must meet certain requirements, summarized in
<a href="tables.html">tables</a>.
*/

/** @defgroup unordered_associative_containers Unordered Associative
 * @ingroup containers
Unordered associative containers allow fast retrieval of data based on keys.

Each container type is parameterized on a @c Key type, a @c Hash type
providing a hashing functor, and an ordering relation used to sort the
elements of the container.

All unordered associative containers must meet certain requirements,
summarized in <a href="tables.html">tables</a>.  */

/**
 * @defgroup diagnostics Diagnostics
 *
 * Components for error handling, reporting, and diagnostic operations.
 */

/**
 * @defgroup concurrency Concurrency
 *
 * Components for concurrent operations, including threads, mutexes,
 * and condition variables.
 */
