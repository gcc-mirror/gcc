// Copyright (C) 2012-2013 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.

// GCC is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

#include <cxxabi.h>
#include <cstdlib>
#include <new>
#include "bits/gthr.h"

#if HAVE___CXA_THREAD_ATEXIT_IMPL

extern "C" int __cxa_thread_atexit_impl (void (*func) (void *),
					 void *arg, void *d);
extern "C" int
__cxxabiv1::__cxa_thread_atexit (void (*dtor)(void *),
				 void *obj, void *dso_handle)
  _GLIBCXX_NOTHROW
{
  return __cxa_thread_atexit_impl (dtor, obj, dso_handle);
}

#else /* HAVE___CXA_THREAD_ATEXIT_IMPL */

namespace {
  // One element in a singly-linked stack of cleanups.
  struct elt
  {
    void (*destructor)(void *);
    void *object;
    elt *next;
  };

  // Keep a per-thread list of cleanups in gthread_key storage.
  __gthread_key_t key;
  // But also support non-threaded mode.
  elt *single_thread;

  // Run the specified stack of cleanups.
  void run (void *p)
  {
    elt *e = static_cast<elt*>(p);
    while (e)
      {
	elt *old_e = e;
	e->destructor (e->object);
	e = e->next;
	delete (old_e);
      }
  }

  // Run the stack of cleanups for the current thread.
  void run ()
  {
    void *e;
    if (__gthread_active_p ())
      {
	e = __gthread_getspecific (key);
	__gthread_setspecific (key, NULL);
      }
    else
      {
	e = single_thread;
	single_thread = NULL;
      }
    run (e);
  }

  // Initialize the key for the cleanup stack.  We use a static local for
  // key init/delete rather than atexit so that delete is run on dlclose.
  void key_init() {
    struct key_s {
      key_s() { __gthread_key_create (&key, run); }
      ~key_s() { __gthread_key_delete (key); }
    };
    static key_s ks;
    // Also make sure the destructors are run by std::exit.
    // FIXME TLS cleanups should run before static cleanups and atexit
    // cleanups.
    std::atexit (run);
  }
}

extern "C" int
__cxxabiv1::__cxa_thread_atexit (void (*dtor)(void *), void *obj, void */*dso_handle*/)
  _GLIBCXX_NOTHROW
{
  // Do this initialization once.
  if (__gthread_active_p ())
    {
      // When threads are active use __gthread_once.
      static __gthread_once_t once = __GTHREAD_ONCE_INIT;
      __gthread_once (&once, key_init);
    }
  else
    {
      // And when threads aren't active use a static local guard.
      static bool queued;
      if (!queued)
	{
	  queued = true;
	  std::atexit (run);
	}
    }

  elt *first;
  if (__gthread_active_p ())
    first = static_cast<elt*>(__gthread_getspecific (key));
  else
    first = single_thread;

  elt *new_elt = new (std::nothrow) elt;
  if (!new_elt)
    return -1;
  new_elt->destructor = dtor;
  new_elt->object = obj;
  new_elt->next = first;

  if (__gthread_active_p ())
    __gthread_setspecific (key, new_elt);
  else
    single_thread = new_elt;

  return 0;
}

#endif /* HAVE___CXA_THREAD_ATEXIT_IMPL */
