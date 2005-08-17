// -*- C++ -*- Manage the thread-local exception globals.
// Copyright (C) 2001, 2004 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING.  If not, write to
// the Free Software Foundation, 51 Franklin Street, Fifth Floor,
// Boston, MA 02110-1301, USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.


#include <exception>
#include <cstdlib>
#include "unwind-cxx.h"
#include "bits/c++config.h"
#include "bits/gthr.h"

using namespace __cxxabiv1;


// Single-threaded fallback buffer.
static __cxa_eh_globals globals_static;

#if __GTHREADS
static __gthread_key_t globals_key;
static int use_thread_key = -1;

static void
get_globals_dtor (void *ptr)
{
  if (ptr)
    {
      __cxa_exception *exn, *next;
      exn = ((__cxa_eh_globals *) ptr)->caughtExceptions;
      while (exn)
	{
	  next = exn->nextException;
	  _Unwind_DeleteException (&exn->unwindHeader);
	  exn = next;
	}
      std::free (ptr);
    }
}

static void
get_globals_init ()
{
  use_thread_key =
    (__gthread_key_create (&globals_key, get_globals_dtor) == 0);
}

static void
get_globals_init_once ()
{
  static __gthread_once_t once = __GTHREAD_ONCE_INIT;
  if (__gthread_once (&once, get_globals_init) != 0
      || use_thread_key < 0)
    use_thread_key = 0;
}
#endif

extern "C" __cxa_eh_globals *
__cxxabiv1::__cxa_get_globals_fast () throw()
{
#if __GTHREADS
  if (use_thread_key)
    return (__cxa_eh_globals *) __gthread_getspecific (globals_key);
  else
    return &globals_static;
#else
  return &globals_static;
#endif
}

extern "C" __cxa_eh_globals *
__cxxabiv1::__cxa_get_globals () throw()
{
#if __GTHREADS
  __cxa_eh_globals *g;

  if (use_thread_key == 0)
    return &globals_static;

  if (use_thread_key < 0)
    {
      get_globals_init_once ();

      // Make sure use_thread_key got initialized.
      if (use_thread_key == 0)
	return &globals_static;
    }

  g = (__cxa_eh_globals *) __gthread_getspecific (globals_key);
  if (! g)
    {
      if ((g = (__cxa_eh_globals *)
	   std::malloc (sizeof (__cxa_eh_globals))) == 0
	  || __gthread_setspecific (globals_key, (void *) g) != 0)
        std::terminate ();
      g->caughtExceptions = 0;
      g->uncaughtExceptions = 0;
    }

  return g;
#else
  return &globals_static;
#endif
}
