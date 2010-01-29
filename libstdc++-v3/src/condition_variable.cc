// condition_variable -*- C++ -*-

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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

namespace std
{
  condition_variable::condition_variable() throw ()
  {
#ifdef __GTHREAD_COND_INIT
    __native_type __tmp = __GTHREAD_COND_INIT;
    _M_cond = __tmp;
#else
    int __e = __gthread_cond_init(&_M_cond, NULL);

    if (__e)
      __throw_system_error(__e);
#endif
  }

  condition_variable::~condition_variable() throw ()
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
  condition_variable::notify_one()
  {
    int __e = __gthread_cond_signal(&_M_cond);

    // XXX not in spec
    // EINVAL
    if (__e)
      __throw_system_error(__e);
  }

  void
  condition_variable::notify_all()
  {
    int __e = __gthread_cond_broadcast(&_M_cond);

    // XXX not in spec
    // EINVAL
    if (__e)
      __throw_system_error(__e);
  }

  condition_variable_any::condition_variable_any() throw ()
  {
#ifdef __GTHREAD_COND_INIT
    __native_type __tmp = __GTHREAD_COND_INIT;
    _M_cond = __tmp;
#else
    int __e = __gthread_cond_init(&_M_cond, NULL);

    if (__e)
      __throw_system_error(__e);
#endif
  }

  condition_variable_any::~condition_variable_any() throw ()
  {
    __gthread_cond_destroy(&_M_cond);
  }

  void
  condition_variable_any::notify_one()
  {
    int __e = __gthread_cond_signal(&_M_cond);

    // XXX not in spec
    // EINVAL
    if (__e)
      __throw_system_error(__e);
  }

  void
  condition_variable_any::notify_all()
  {
    int __e = __gthread_cond_broadcast(&_M_cond);

    // XXX not in spec
    // EINVAL
    if (__e)
      __throw_system_error(__e);
  }
}

#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1
