// <memory> -*- C++ -*-

// Copyright (C) 2001, 2002 Free Software Foundation, Inc.
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
 * Copyright (c) 1997-1999
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */

/** @file memory
 *  This is a Standard C++ Library header.  You should @c #include this header
 *  in your programs, rather than any of the "st[dl]_*.h" implementation files.
 */

#ifndef _CPP_MEMORY
#define _CPP_MEMORY 1

#pragma GCC system_header

#include <bits/stl_algobase.h>
#include <bits/stl_alloc.h>
#include <bits/stl_construct.h>
#include <bits/stl_iterator_base_types.h> //for iterator_traits
#include <bits/stl_uninitialized.h>
#include <bits/stl_raw_storage_iter.h>

namespace std
{

  /**
   *  @if maint
   *  This is a helper function.  The unused second parameter exists to
   *  permit the real get_temporary_buffer to use template parameter deduction.
   *  @endif
  */
  template <class _Tp>
  pair<_Tp*, ptrdiff_t> 
  __get_temporary_buffer(ptrdiff_t __len, _Tp*)
  {
    if (__len > ptrdiff_t(INT_MAX / sizeof(_Tp)))
      __len = INT_MAX / sizeof(_Tp);

    while (__len > 0) {
      _Tp* __tmp = (_Tp*) std::malloc((std::size_t)__len * sizeof(_Tp));
      if (__tmp != 0)
	return pair<_Tp*, ptrdiff_t>(__tmp, __len);
      __len /= 2;
    }

    return pair<_Tp*, ptrdiff_t>((_Tp*)0, 0);
  }

  /**
   *  @brief This is a mostly-useless wrapper around malloc().
   *  @param  len  The number of objects of type Tp.
   *  @return   See full description.
   *
   *  Reinventing the wheel, but this time with prettier spokes!
   *
   *  This function tries to obtain storage for @c len adjacent Tp objects.
   *  The objects themselves are not constructed, of course.  A pair<> is
   *  returned containing "the buffer s address and capacity (in the units of
   *  sizeof(Tp)), or a pair of 0 values if no storage can be obtained."
   *  Note that the capacity obtained may be less than that requested if the
   *  memory is unavailable; you should compare len with the .second return
   *  value.
  */
  template <class _Tp>
  inline pair<_Tp*, ptrdiff_t> get_temporary_buffer(ptrdiff_t __len) {
    return __get_temporary_buffer(__len, (_Tp*) 0);
  }

  /**
   *  @brief The companion to get_temporary_buffer().
   *  @param  p  A buffer previously allocated by get_temporary_buffer.
   *  @return   None.
   *
   *  Frees the memory pointed to by p.
   */
  template <class _Tp>
  void return_temporary_buffer(_Tp* __p) {
    std::free(__p);
  }


template <class _Tp1>
  struct auto_ptr_ref
{
   _Tp1* _M_ptr;
   auto_ptr_ref(_Tp1* __p) : _M_ptr(__p) {}
};

/**
 *  A simple smart pointer providing strict ownership semantics.  (More later.)
*/
template <class _Tp>
  class auto_ptr
{
private:
  _Tp* _M_ptr;

public:
  typedef _Tp element_type;

  explicit auto_ptr(_Tp* __p = 0) throw() : _M_ptr(__p) {}
  auto_ptr(auto_ptr& __a) throw() : _M_ptr(__a.release()) {}

  template <class _Tp1> auto_ptr(auto_ptr<_Tp1>& __a) throw()
    : _M_ptr(__a.release()) {}

  auto_ptr& operator=(auto_ptr& __a) throw() {
    reset(__a.release());
    return *this;
  }

  template <class _Tp1>
  auto_ptr& operator=(auto_ptr<_Tp1>& __a) throw() {
    reset(__a.release());
    return *this;
  }
  
  // Note: The C++ standard says there is supposed to be an empty throw
  // specification here, but omitting it is standard conforming.  Its 
  // presence can be detected only if _Tp::~_Tp() throws, but (17.4.3.6/2)
  // this is prohibited.
  ~auto_ptr() { delete _M_ptr; }
 
  _Tp& operator*() const throw() {
    return *_M_ptr;
  }
  _Tp* operator->() const throw() {
    return _M_ptr;
  }
  _Tp* get() const throw() {
    return _M_ptr;
  }
  _Tp* release() throw() {
    _Tp* __tmp = _M_ptr;
    _M_ptr = 0;
    return __tmp;
  }
  void reset(_Tp* __p = 0) throw() {
    if (__p != _M_ptr) {
      delete _M_ptr;
      _M_ptr = __p;
    }    
  }

public:
  auto_ptr(auto_ptr_ref<_Tp> __ref) throw()
    : _M_ptr(__ref._M_ptr) {}

  auto_ptr& operator=(auto_ptr_ref<_Tp> __ref) throw() {
    if (__ref._M_ptr != this->get()) {
      delete _M_ptr;
      _M_ptr = __ref._M_ptr;
    }
    return *this;
  }

  template <class _Tp1> operator auto_ptr_ref<_Tp1>() throw() 
    { return auto_ptr_ref<_Tp>(this->release()); }
  template <class _Tp1> operator auto_ptr<_Tp1>() throw()
    { return auto_ptr<_Tp1>(this->release()); }
};

} // namespace std

#endif /* _CPP_MEMORY */

