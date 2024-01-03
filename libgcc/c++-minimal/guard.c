/* 'libstdc++-v3/libsupc++/guard.cc' for offload devices, until we have
   libstdc++-v3/libsupc++ proper.

   Copyright (C) 2002-2024 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#if defined __AMDGCN__
#elif defined __nvptx__
#else
# error not ported
#endif

#include "../../libstdc++-v3/config/cpu/generic/cxxabi_tweaks.h"

/* Copy'n'paste/edit from 'libstdc++-v3/libsupc++/cxxabi.h'.  */

int
__cxa_guard_acquire(__guard*);

void
__cxa_guard_release(__guard*);

void
__cxa_guard_abort(__guard*);

/* Copy'n'paste/edit from 'libstdc++-v3/libsupc++/guard.cc'.  */

#undef _GLIBCXX_GUARD_TEST_AND_ACQUIRE
#undef _GLIBCXX_GUARD_SET_AND_RELEASE
#define _GLIBCXX_GUARD_SET_AND_RELEASE(G) _GLIBCXX_GUARD_SET (G)

static inline int
init_in_progress_flag(__guard* g)
{ return ((char *)g)[1]; }

static inline void
set_init_in_progress_flag(__guard* g, int v)
{ ((char *)g)[1] = v; }

static inline void
throw_recursive_init_exception(void)
{
  // Use __builtin_trap so we don't require abort().
  __builtin_trap();
}

// acquire() is a helper function used to acquire guard if thread support is
// not compiled in or is compiled in but not enabled at run-time.
static int
acquire(__guard *g)
{
  // Quit if the object is already initialized.
  if (_GLIBCXX_GUARD_TEST(g))
    return 0;

  if (init_in_progress_flag(g))
    throw_recursive_init_exception();

  set_init_in_progress_flag(g, 1);
  return 1;
}

int __cxa_guard_acquire (__guard *g)
{
  return acquire (g);
}

void __cxa_guard_abort (__guard *g)
{
  set_init_in_progress_flag(g, 0);
}

void __cxa_guard_release (__guard *g)
{
  set_init_in_progress_flag(g, 0);
  _GLIBCXX_GUARD_SET_AND_RELEASE (g);
}
