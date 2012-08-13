// condition_variable -*- C++ -*-

// Copyright (C) 2008, 2009, 2010, 2012 Free Software Foundation, Inc.
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

#include <condition_variable>

#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

#ifdef __GTHREAD_COND_INIT
  condition_variable::condition_variable() noexcept = default;
#else
  condition_variable::condition_variable() noexcept
  {
    __GTHREAD_COND_INIT_FUNCTION(&_M_cond);
  }
#endif

  condition_variable::~condition_variable() noexcept
  {
    // XXX no thread blocked
    /* int __e = */ __gthread_cond_destroy(&_M_cond);
    // if __e == EBUSY then blocked
  }

  void
  condition_variable::wait(unique_lock<mutex>& __lock)
  {
    int __e = __gthread_cond_wait(&_M_cond, __lock.mutex()->native_handle());

    if (__e)
      __throw_system_error(__e);
  }

  void
  condition_variable::notify_one() noexcept
  {
    int __e = __gthread_cond_signal(&_M_cond);

    // XXX not in spec
    // EINVAL
    if (__e)
      __throw_system_error(__e);
  }

  void
  condition_variable::notify_all() noexcept
  {
    int __e = __gthread_cond_broadcast(&_M_cond);

    // XXX not in spec
    // EINVAL
    if (__e)
      __throw_system_error(__e);
  }

  condition_variable_any::condition_variable_any() noexcept = default;

  condition_variable_any::~condition_variable_any() noexcept = default;

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace

#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1
