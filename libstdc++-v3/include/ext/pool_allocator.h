// Allocators -*- C++ -*-

// Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

/*
 * Copyright (c) 1996-1997
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 */

/** @file ext/pool_allocator.h
 *  This file is a GNU extension to the Standard C++ Library.
 *  You should only include this header if you are using GCC 3 or later.
 */
#ifndef _POOL_ALLOCATOR_H
#define _POOL_ALLOCATOR_H 1

#include <bits/c++config.h>
#include <new>
#include <bits/functexcept.h>
#include <bits/stl_threads.h>
#include <bits/atomicity.h>

namespace __gnu_cxx
{
  using std::__throw_bad_alloc;

  /**
   *  @if maint
   *  Uses various allocators to fulfill underlying requests (and makes as
   *  few requests as possible when in default high-speed pool mode).
   *
   *  Important implementation properties:
   *  0. If globally mandated, then allocate objects from new
   *  1. If the clients request an object of size > _S_max_bytes, the resulting
   *     object will be obtained directly from new
   *  2. In all other cases, we allocate an object of size exactly
   *     _S_round_up(requested_size).  Thus the client has enough size
   *     information that we can return the object to the proper free list
   *     without permanently losing part of the object.
   *
   *  The template parameter specifies whether more than one thread may use
   *  this allocator.  It is safe to allocate an object from one instance
   *  of the allocator and deallocate it with another one.  This effectively
   *  transfers its ownership to the second one.  This may have undesirable
   *  effects on reference locality.
   *
   *  @endif
   *  (See @link Allocators allocators info @endlink for more.)
   */
  template<bool __threads>
    struct __pool_base
    {
      enum { _S_align = 8 };
      enum { _S_max_bytes = 128 };
      enum { _S_freelists = _S_max_bytes / _S_align };
      
      union _Obj
      {
	union _Obj* _M_free_list_link;
	char        _M_client_data[1];    // The client sees this.
      };
      
      static _Obj* volatile         _S_free_list[_S_freelists];
      
      // Chunk allocation state.
      static char*                  _S_start_free;
      static char*                  _S_end_free;
      static size_t                 _S_heap_size;
      
      static _STL_mutex_lock        _S_lock;
      static _Atomic_word	    _S_force_new;
      
      static size_t
      _S_round_up(size_t __bytes)
      { return ((__bytes + (size_t)_S_align - 1) & ~((size_t)_S_align - 1)); }
      
      static size_t
      _S_freelist_index(size_t __bytes)
      { return ((__bytes + (size_t)_S_align - 1) / (size_t)_S_align - 1); }
    
      // Returns an object of size __n, and optionally adds to size __n
      // free list.
      static void*
      _S_refill(size_t __n);
      
      // Allocates a chunk for nobjs of size size.  nobjs may be reduced
      // if it is inconvenient to allocate the requested number.
      static char*
      _S_chunk_alloc(size_t __n, int& __nobjs);
      
      // It would be nice to use _STL_auto_lock here.  But we need a
      // test whether threads are in use.
      struct _Lock
      {
	_Lock() { if (__threads) _S_lock._M_acquire_lock(); }
	~_Lock() { if (__threads) _S_lock._M_release_lock(); }
      } __attribute__ ((__unused__));
      friend struct _Lock;
    };

  typedef __pool_base<true> __pool_alloc_base;

  template<typename _Tp>
    class __pool_alloc : private __pool_alloc_base
    {
    public:
      typedef size_t     size_type;
      typedef ptrdiff_t  difference_type;
      typedef _Tp*       pointer;
      typedef const _Tp* const_pointer;
      typedef _Tp&       reference;
      typedef const _Tp& const_reference;
      typedef _Tp        value_type;

      template<typename _Tp1>
        struct rebind
        { typedef __pool_alloc<_Tp1> other; };

      __pool_alloc() throw() { }

      __pool_alloc(const __pool_alloc&) throw() { }

      template<typename _Tp1>
        __pool_alloc(const __pool_alloc<_Tp1>&) throw() { }

      ~__pool_alloc() throw() { }

      pointer
      address(reference __x) const { return &__x; }

      const_pointer
      address(const_reference __x) const { return &__x; }

      size_type
      max_size() const throw() 
      { return size_t(-1) / sizeof(_Tp); }

      // _GLIBCXX_RESOLVE_LIB_DEFECTS
      // 402. wrong new expression in [some_] allocator::construct
      void 
      construct(pointer __p, const _Tp& __val) 
      { ::new(__p) _Tp(__val); }

      void 
      destroy(pointer __p) { __p->~_Tp(); }

      pointer
      allocate(size_type __n, const void* = 0);

      void
      deallocate(pointer __p, size_type __n);      
    };

  template<typename _Tp>
    inline bool
    operator==(const __pool_alloc<_Tp>&, const __pool_alloc<_Tp>&)
    { return true; }

  template<typename _Tp>
    inline bool
    operator!=(const __pool_alloc<_Tp>&, const __pool_alloc<_Tp>&)
    { return false; }

  // Allocate memory in large chunks in order to avoid fragmenting the
  // heap too much.  Assume that __n is properly aligned.  We hold
  // the allocation lock.
  template<bool __threads>
    char*
    __pool_base<__threads>::_S_chunk_alloc(size_t __n, int& __nobjs)
    {
      char* __result;
      size_t __total_bytes = __n * __nobjs;
      size_t __bytes_left = _S_end_free - _S_start_free;
      
      if (__bytes_left >= __total_bytes)
	{
	  __result = _S_start_free;
	  _S_start_free += __total_bytes;
	  return __result ;
	}
      else if (__bytes_left >= __n)
	{
	  __nobjs = (int)(__bytes_left / __n);
	  __total_bytes = __n * __nobjs;
	  __result = _S_start_free;
	  _S_start_free += __total_bytes;
	  return __result;
	}
      else
	{
	  size_t __bytes_to_get = (2 * __total_bytes
				   + _S_round_up(_S_heap_size >> 4));
	  // Try to make use of the left-over piece.
	  if (__bytes_left > 0)
	    {
	      _Obj* volatile* __free_list = (_S_free_list
					     + _S_freelist_index(__bytes_left));
	      
	      ((_Obj*)(void*)_S_start_free)->_M_free_list_link = *__free_list;
	      *__free_list = (_Obj*)(void*)_S_start_free;
	    }
	  
	  _S_start_free = static_cast<char*>(::operator new(__bytes_to_get));
	  if (_S_start_free == 0)
	    {
	      size_t __i;
	      _Obj* volatile* __free_list;
	      _Obj* __p;
	      // Try to make do with what we have.  That can't hurt.  We
	      // do not try smaller requests, since that tends to result
	      // in disaster on multi-process machines.
	      __i = __n;
	      for (; __i <= (size_t) _S_max_bytes; __i += (size_t) _S_align)
		{
		  __free_list = _S_free_list + _S_freelist_index(__i);
		  __p = *__free_list;
		  if (__p != 0)
		    {
		      *__free_list = __p -> _M_free_list_link;
		      _S_start_free = (char*)__p;
		      _S_end_free = _S_start_free + __i;
		      return _S_chunk_alloc(__n, __nobjs);
		      // Any leftover piece will eventually make it to the
		      // right free list.
		    }
		}
	      _S_end_free = 0;        // In case of exception.
	      _S_start_free = static_cast<char*>(::operator new(__bytes_to_get));
	      // This should either throw an exception or remedy the situation.
	      // Thus we assume it succeeded.
	    }
	  _S_heap_size += __bytes_to_get;
	  _S_end_free = _S_start_free + __bytes_to_get;
	  return _S_chunk_alloc(__n, __nobjs);
	}
    }
  
  // Returns an object of size __n, and optionally adds to "size
  // __n"'s free list.  We assume that __n is properly aligned.  We
  // hold the allocation lock.
  template<bool __threads>
    void*
    __pool_base<__threads>::_S_refill(size_t __n)
    {
      int __nobjs = 20;
      char* __chunk = _S_chunk_alloc(__n, __nobjs);
      _Obj* volatile* __free_list;
      _Obj* __result;
      _Obj* __current_obj;
      _Obj* __next_obj;
      int __i;
      
      if (1 == __nobjs)
	return __chunk;
      __free_list = _S_free_list + _S_freelist_index(__n);
      
      // Build free list in chunk.
      __result = (_Obj*)(void*)__chunk;
      *__free_list = __next_obj = (_Obj*)(void*)(__chunk + __n);
      for (__i = 1; ; __i++)
	{
	  __current_obj = __next_obj;
	  __next_obj = (_Obj*)(void*)((char*)__next_obj + __n);
	  if (__nobjs - 1 == __i)
	    {
	      __current_obj -> _M_free_list_link = 0;
	      break;
	    }
	  else
	    __current_obj -> _M_free_list_link = __next_obj;
	}
      return __result;
    }

  template<typename _Tp>
    _Tp*
    __pool_alloc<_Tp>::allocate(size_type __n, const void*)
    {
      pointer __ret = 0;
      if (__n)
	{
	  if (__n <= max_size())
	    {
	      const size_t __bytes = __n * sizeof(_Tp);
	      // If there is a race through here, assume answer from getenv
	      // will resolve in same direction.  Inspired by techniques
	      // to efficiently support threading found in basic_string.h.
	      if (_S_force_new == 0)
		{
		  if (getenv("GLIBCXX_FORCE_NEW"))
		    __atomic_add(&_S_force_new, 1);
		  else
		    __atomic_add(&_S_force_new, -1);
		}
	      
	      if ((__bytes > (size_t) _S_max_bytes) || (_S_force_new > 0))
		__ret = static_cast<_Tp*>(::operator new(__bytes));
	      else
		{
		  _Obj* volatile* __free_list = (_S_free_list
						 + _S_freelist_index(__bytes));
		  // Acquire the lock here with a constructor call.  This
		  // ensures that it is released in exit or during stack
		  // unwinding.
		  _Lock __lock_instance;
		  _Obj* __restrict__ __result = *__free_list;
		  if (__builtin_expect(__result == 0, 0))
		    __ret = static_cast<_Tp*>(_S_refill(_S_round_up(__bytes)));
		  else
		    {
		      *__free_list = __result->_M_free_list_link;
		      __ret = reinterpret_cast<_Tp*>(__result);
		    }
		  if (__builtin_expect(__ret == 0, 0))
		    __throw_bad_alloc();
		}
	    }
	  else
	    __throw_bad_alloc();
	}
      return __ret;
    }

  template<typename _Tp>
    void
    __pool_alloc<_Tp>::deallocate(pointer __p, size_type __n)
    {
      if (__n)
	{
	  const size_t __bytes = __n * sizeof(_Tp);
	  if ((__bytes > (size_t) _S_max_bytes) || (_S_force_new > 0))
	    ::operator delete(__p);
	  else
	    {
	      _Obj* volatile* __free_list = (_S_free_list
					     + _S_freelist_index(__bytes));
	      _Obj* __q = (_Obj*)__p;

	      // Acquire the lock here with a constructor call.  This
	      // ensures that it is released in exit or during stack
	      // unwinding.
	      _Lock __lock_instance;
	      __q -> _M_free_list_link = *__free_list;
	      *__free_list = __q;
	    }
	}
    }

  template<bool __threads>
    typename __pool_base<__threads>::_Obj* volatile
    __pool_base<__threads>::_S_free_list[_S_freelists];

  template<bool __threads>
    char* __pool_base<__threads>::_S_start_free = 0;

  template<bool __threads>
    char* __pool_base<__threads>::_S_end_free = 0;

  template<bool __threads>
    size_t __pool_base<__threads>::_S_heap_size = 0;

  template<bool __threads>
    _STL_mutex_lock
    __pool_base<__threads>::_S_lock __STL_MUTEX_INITIALIZER;

  template<bool __threads>
    _Atomic_word
    __pool_base<__threads>::_S_force_new = 0;
} // namespace __gnu_cxx

#endif
