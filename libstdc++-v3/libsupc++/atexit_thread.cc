// Copyright (C) 2012 Free Software Foundation, Inc.
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

namespace {
  // Data structure for the list of destructors: Singly-linked list
  // of arrays.
  class list
  {
    struct elt
    {
      void *object;
      void (*destructor)(void *);
    };

    static const int max_nelts = 32;

    list *next;
    int nelts;
    elt array[max_nelts];

    elt *allocate_elt();
  public:
    void run();
    static void run(void *p);
    int add_elt(void (*)(void *), void *);
  };

  // Return the address of an open slot.
  list::elt *
  list::allocate_elt()
  {
    if (nelts < max_nelts)
      return &array[nelts++];
    if (!next)
      next = new (std::nothrow) list();
    if (!next)
      return 0;
    return next->allocate_elt();
  }

  // Run all the cleanups in the list.
  void
  list::run()
  {
    for (int i = nelts - 1; i >= 0; --i)
      array[i].destructor (array[i].object);
    if (next)
      next->run();
  }

  // Static version to use as a callback to __gthread_key_create.
  void
  list::run(void *p)
  {
    static_cast<list *>(p)->run();
  }

  // The list of cleanups is per-thread.
  thread_local list first;

  // The pthread data structures for actually running the destructors at
  // thread exit are shared.  The constructor of the thread-local sentinel
  // object in add_elt performs the initialization.
  __gthread_key_t key;
  __gthread_once_t once = __GTHREAD_ONCE_INIT;
  void run_current () { first.run(); }
  void key_init() {
    __gthread_key_create (&key, list::run);
    // Also make sure the destructors are run by std::exit.
    // FIXME TLS cleanups should run before static cleanups and atexit
    // cleanups.
    std::atexit (run_current);
  }
  struct sentinel
  {
    sentinel()
    {
      if (__gthread_active_p ())
	{
	  __gthread_once (&once, key_init);
	  __gthread_setspecific (key, &first);
	}
      else
	std::atexit (run_current);
    }
  };

  // Actually insert an element.
  int
  list::add_elt(void (*dtor)(void *), void *obj)
  {
    thread_local sentinel s;
    elt *e = allocate_elt ();
    if (!e)
      return -1;
    e->object = obj;
    e->destructor = dtor;
    return 0;
  }
}

namespace __cxxabiv1
{
  extern "C" int
  __cxa_thread_atexit (void (*dtor)(void *), void *obj, void */*dso_handle*/)
    _GLIBCXX_NOTHROW
  {
    return first.add_elt (dtor, obj);
  }
}
