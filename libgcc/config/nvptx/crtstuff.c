/* Copyright (C) 2022 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 3, or (at your option) any
   later version.

   This file is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#include "gbl-ctors.h"

/* The common 'crtstuff.c' doesn't quite provide what we need, so we roll our
   own.

   There's no technical reason in this configuration here to split the two
   functions '__do_global_ctors' and '__do_global_ctors' into two separate
   files (via 'CRT_BEGIN' and 'CRT_END'): 'crtbegin.o' and 'crtend.o', but we
   do so anyway, for symmetry with other configurations.  */

#ifdef CRT_BEGIN

void
__do_global_ctors (void)
{
  DO_GLOBAL_CTORS_BODY;
}

#elif defined(CRT_END) /* ! CRT_BEGIN */

void
__do_global_dtors (void)
{
  /* In this configuration here, there's no way that "this routine is run more
     than once [...] when exit is called recursively": for nvptx target, the
     call to '__do_global_dtors' is registered via 'atexit', which doesn't
     re-enter a function already run.
     Therefore, we do *not* "arrange to remember where in the list we left off
     processing".  */
  func_ptr *p;
  for (p = __DTOR_LIST__ + 1; *p; )
    (*p++) ();
}

#else /* ! CRT_BEGIN && ! CRT_END */
#error "One of CRT_BEGIN or CRT_END must be defined."
#endif
