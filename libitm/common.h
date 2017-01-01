/* Copyright (C) 2008-2017 Free Software Foundation, Inc.
   Contributed by Richard Henderson <rth@redhat.com>.

   This file is part of the GNU Transactional Memory Library (libitm).

   Libitm is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   Libitm is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* The following are internal implementation functions and definitions.
   To distinguish them from those defined by the Intel ABI, they all
   begin with GTM/gtm.  */

#ifndef COMMON_H
#define COMMON_H 1

#define UNUSED		__attribute__((unused))
#define ALWAYS_INLINE	__attribute__((always_inline))
#ifdef HAVE_ATTRIBUTE_VISIBILITY
# define HIDDEN		__attribute__((visibility("hidden")))
#else
# define HIDDEN
#endif

#define likely(X)	__builtin_expect((X) != 0, 1)
#define unlikely(X)	__builtin_expect((X), 0)

namespace GTM HIDDEN {

// Locally defined protected allocation functions.
//
// To avoid dependency on libstdc++ new/delete, as well as to not
// interfere with the wrapping of the global new/delete we wrap for
// the user in alloc_cpp.cc, use class-local versions that defer
// to malloc/free.  Recall that operator new/delete does not go through
// normal lookup and so we cannot simply inject a version into the
// GTM namespace.
// If separate_cl is true, the allocator will try to return memory that is on
// cache lines that are not shared with any object used by another thread.
extern void * xmalloc (size_t s, bool separate_cl = false)
  __attribute__((malloc, nothrow));
extern void * xcalloc (size_t s, bool separate_cl = false)
  __attribute__((malloc, nothrow));
extern void * xrealloc (void *p, size_t s, bool separate_cl = false)
  __attribute__((malloc, nothrow));

} // namespace GTM


#endif // COMMON_H
