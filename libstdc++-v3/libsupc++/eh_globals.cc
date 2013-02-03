// -*- C++ -*- Manage the thread-local exception globals.
// Copyright (C) 2001-2013 Free Software Foundation, Inc.
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

#include <bits/c++config.h>
#include <exception>
#include <cstdlib>
#include "cxxabi.h"
#include "unwind-cxx.h"
#include "bits/gthr.h"

#if _GLIBCXX_HOSTED
using std::free;
using std::malloc;
#else
// In a freestanding environment, these functions may not be
// available -- but for now, we assume that they are.
extern "C" void *malloc (std::size_t);
extern "C" void free(void *);
#endif

using namespace __cxxabiv1;

#if _GLIBCXX_HAVE_TLS

namespace
{
  abi::__cxa_eh_globals*
  get_global() _GLIBCXX_NOTHROW
  {
    static __thread abi::__cxa_eh_globals global;
    return &global;
  }
} // anonymous namespace

extern "C" __cxa_eh_globals*
__cxxabiv1::__cxa_get_globals_fast() _GLIBCXX_NOTHROW
{ return get_global(); }

extern "C" __cxa_eh_globals*
__cxxabiv1::__cxa_get_globals() _GLIBCXX_NOTHROW
{ return get_global(); }


#else

// Single-threaded fallback buffer.
static __cxa_eh_globals eh_globals;

#if __GTHREADS

static void
eh_globals_dtor(void* ptr)
{
  if (ptr)
    {
      __cxa_eh_globals* g = reinterpret_cast<__cxa_eh_globals*>(ptr);
      __cxa_exception* exn = g->caughtExceptions;
      __cxa_exception* next;
      while (exn)
	{
	  next = exn->nextException;
	  _Unwind_DeleteException(&exn->unwindHeader);
	  exn = next;
	}
      free(ptr);
    }
}

struct __eh_globals_init
{
  __gthread_key_t  	_M_key;
  bool 			_M_init;

  __eh_globals_init() : _M_init(false)
  { 
    if (__gthread_active_p())
      _M_init = __gthread_key_create(&_M_key, eh_globals_dtor) == 0; 
  }

  ~__eh_globals_init()
  {
    if (_M_init)
      __gthread_key_delete(_M_key);
    _M_init = false;
  }
};

static __eh_globals_init init;

extern "C" __cxa_eh_globals*
__cxxabiv1::__cxa_get_globals_fast() _GLIBCXX_NOTHROW
{
  __cxa_eh_globals* g;
  if (init._M_init)
    g = static_cast<__cxa_eh_globals*>(__gthread_getspecific(init._M_key));
  else
    g = &eh_globals;
  return g;
}

extern "C" __cxa_eh_globals*
__cxxabiv1::__cxa_get_globals() _GLIBCXX_NOTHROW
{
  __cxa_eh_globals* g;
  if (init._M_init)
    {
      g = static_cast<__cxa_eh_globals*>(__gthread_getspecific(init._M_key));
      if (!g)
	{
	  void* v = malloc(sizeof(__cxa_eh_globals));
	  if (v == 0 || __gthread_setspecific(init._M_key, v) != 0)
	    std::terminate();
	  g = static_cast<__cxa_eh_globals*>(v);
	  g->caughtExceptions = 0;
	  g->uncaughtExceptions = 0;
#ifdef __ARM_EABI_UNWINDER__
	  g->propagatingExceptions = 0;
#endif
	}
    }
  else
    g = &eh_globals;
  return g;
}

#else

extern "C" __cxa_eh_globals*
__cxxabiv1::__cxa_get_globals_fast() _GLIBCXX_NOTHROW
{ return &eh_globals; }

extern "C" __cxa_eh_globals*
__cxxabiv1::__cxa_get_globals() _GLIBCXX_NOTHROW
{ return &eh_globals; }

#endif

#endif
