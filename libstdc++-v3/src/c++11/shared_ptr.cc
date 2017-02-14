// Support for pointer abstractions -*- C++ -*-

// Copyright (C) 2011-2017 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
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

#include <memory>

#include "mutex_pool.h"

namespace __gnu_internal _GLIBCXX_VISIBILITY(hidden)
{
  /* Returns different instances of __mutex depending on the passed index
   * in order to limit contention.
   */
  __gnu_cxx::__mutex&
  get_mutex(unsigned char i)
  {
    static __gnu_cxx::__mutex m[mask + 1];
    return m[i];
  }
}

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  bad_weak_ptr::~bad_weak_ptr() noexcept = default;

  char const*
  bad_weak_ptr::what() const noexcept
  { return "bad_weak_ptr"; }

#ifdef __GTHREADS
  namespace
  {
    inline unsigned char key(const void* addr)
    { return _Hash_impl::hash(addr) & __gnu_internal::mask; }
  }

  _Sp_locker::_Sp_locker(const void* p) noexcept
  {
    if (__gthread_active_p())
      {
	_M_key1 = _M_key2 = key(p);
        __gnu_internal::get_mutex(_M_key1).lock();
      }
    else
      _M_key1 = _M_key2 = __gnu_internal::invalid;
  }

  _Sp_locker::_Sp_locker(const void* p1, const void* p2) noexcept
  {
    if (__gthread_active_p())
      {
	_M_key1 = key(p1);
	_M_key2 = key(p2);
	if (_M_key2 < _M_key1)
	  __gnu_internal::get_mutex(_M_key2).lock();
	__gnu_internal::get_mutex(_M_key1).lock();
	if (_M_key2 > _M_key1)
	  __gnu_internal::get_mutex(_M_key2).lock();
      }
    else
      _M_key1 = _M_key2 = __gnu_internal::invalid;
  }

  _Sp_locker::~_Sp_locker()
  {
    if (_M_key1 != __gnu_internal::invalid)
      {
	__gnu_internal::get_mutex(_M_key1).unlock();
	if (_M_key2 != _M_key1)
	  __gnu_internal::get_mutex(_M_key2).unlock();
      }
  }
#endif

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace
