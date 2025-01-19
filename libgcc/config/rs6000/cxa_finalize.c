/* Copyright (C) 1999-2025 Free Software Foundation, Inc.

   NOTE: This source is derived from an old version taken from the GNU C
   Library (glibc).

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

#include <assert.h>
#include <stdlib.h>
#include "exit.h"


static boolean_t
catomic_compare_and_exchange_bool_acq (long *mem, long newval, long oldval)
{
  return ! __atomic_compare_exchange (mem, &oldval, &newval, 0,
				      __ATOMIC_ACQUIRE, __ATOMIC_ACQUIRE);
}

/* If D is non-NULL, call all functions registered with `__cxa_atexit'
   with the same dso handle.  Otherwise, if D is NULL, call all of the
   registered handlers.  */
void
__cxa_finalize (void *d)
{
  struct exit_function_list *funcs;

 restart:
  for (funcs = __exit_funcs; funcs; funcs = funcs->next)
    {
      struct exit_function *f;

      for (f = &funcs->fns[funcs->idx - 1]; f >= &funcs->fns[0]; --f)
	{
	  void (*cxafn) (void *arg, int status);
	  void *cxaarg;

	  if ((d == NULL || d == f->func.cxa.dso_handle)
	      /* We don't want to run this cleanup more than once.  */
	      && (cxafn = f->func.cxa.fn,
		  cxaarg = f->func.cxa.arg,
		  ! catomic_compare_and_exchange_bool_acq (&f->flavor, ef_free,
							   ef_cxa)))
	    {
	      uint64_t check = __new_exitfn_called;

#ifdef PTR_DEMANGLE
	      PTR_DEMANGLE (cxafn);
#endif
	      cxafn (cxaarg, 0);

	      /* It is possible that that last exit function registered
		 more exit functions.  Start the loop over.  */
	      if (__builtin_expect (check != __new_exitfn_called, 0))
		goto restart;
	    }
	}
    }

  /* Remove the registered fork handlers.  We do not have to
     unregister anything if the program is going to terminate anyway.  */
#ifdef UNREGISTER_ATFORK
  if (d != NULL)
    UNREGISTER_ATFORK (d);
#endif
}
