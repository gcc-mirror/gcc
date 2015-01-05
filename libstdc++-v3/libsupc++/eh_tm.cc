// -*- C++ -*- Exception handling routines for Transactional Memory.
// Copyright (C) 2009-2015 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <cstdlib>
#include "unwind-cxx.h"

using namespace __cxxabiv1;

// Free one C++ exception.

static void
free_any_cxa_exception (_Unwind_Exception *eo)
{
  __cxa_refcounted_exception *h
    = __get_refcounted_exception_header_from_ue (eo);

  if (__is_dependent_exception (eo->exception_class))
    {
      __cxa_dependent_exception *dep
	= __get_dependent_exception_from_ue (eo);

      h = __get_refcounted_exception_header_from_obj (dep->primaryException);

      __cxa_free_dependent_exception (dep);
    }

#if __GCC_ATOMIC_INT_LOCK_FREE > 1
  if (__atomic_sub_fetch (&h->referenceCount, 1, __ATOMIC_ACQ_REL) == 0)
#endif
    __cxa_free_exception (h + 1);
}

// Cleanup exception handling state while rolling back state for
// a software transactional memory transaction.
//
// UNTHROWN_OBJ is non-null if we've called __cxa_allocate_exception
// but not yet called __cxa_throw for it.
//
// CLEANUP_EXC is non-null if we're currently processing a cleanup
// along an exception path, but we've not caught the exception yet.
//
// CAUGHT_COUNT is the nesting depth of __cxa_begin_catch within
// the transaction; undo as if calling __cxa_end_catch that many times.

extern "C" void
__cxxabiv1::__cxa_tm_cleanup (void *unthrown_obj,
			      void *cleanup_exc,
			      unsigned int caught_count) throw()
{
  __cxa_eh_globals *globals = __cxa_get_globals_fast ();

  // Handle a C++ exception not yet thrown.
  if (unthrown_obj)
    {
      globals->uncaughtExceptions -= 1;
      __cxa_free_exception (unthrown_obj);
    }

  // Handle an exception not yet caught ie. processing a cleanup
  // in between the throw and the catch.
  if (cleanup_exc)
    {
      _Unwind_Exception *eo
	= reinterpret_cast <_Unwind_Exception *>(cleanup_exc);
      if (__is_gxx_exception_class (eo->exception_class))
	free_any_cxa_exception (eo);
      else
	_Unwind_DeleteException (eo);
    }

  // Do __cxa_end_catch caught_count times, but don't bother running
  // the destructors for the objects involved.  All of that is being
  // undone by the transaction restart.
  if (caught_count > 0)
    {
      __cxa_exception *h = globals->caughtExceptions;

      // Rethrown foreign exceptions are removed from the stack immediately.
      // We would have freed this exception via THIS_EXC above.
      if (h == NULL)
	return;

      do
	{
	  __cxa_exception *next;
	  _Unwind_Exception *eo = &h->unwindHeader;

	  if (__is_gxx_exception_class (eo->exception_class))
	    {
	      next = h->nextException;
	      free_any_cxa_exception (eo);
	    }
	  else
	    {
	      _Unwind_DeleteException (eo);
	      next = 0;
	    }

	  h = next;
	}
      while (--caught_count);

      globals->caughtExceptions = h;
    }
}
