// condition_variable -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <condition_variable>

#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)

namespace std
{
  condition_variable::condition_variable()
  {
#ifdef __GTHREAD_COND_INIT
    __gthread_cond_t __tmp = __GTHREAD_COND_INIT;
    _M_cond = __tmp;
#else
    int __e = __gthread_cond_init(&_M_cond, NULL);

    if (__e)
      __throw_system_error(__e);
#endif
  }

  condition_variable::~condition_variable()
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
    lock_guard<mutex> __lock(_M_internal_mutex);
    int __e = __gthread_cond_signal(&_M_cond);

    // XXX not in spec
    // EINVAL
    if (__e)
      __throw_system_error(__e);
  }

  void 
  condition_variable::notify_all()
  { 
    lock_guard<mutex> __lock(_M_internal_mutex);
    int __e = __gthread_cond_broadcast(&_M_cond);

    // XXX not in spec
    // EINVAL
    if (__e)
      __throw_system_error(__e);
  }

  condition_variable_any::condition_variable_any()
  {
#ifdef __GTHREAD_COND_INIT
    __gthread_cond_t __tmp = __GTHREAD_COND_INIT;
    _M_cond = __tmp;
#else
    int __e = __gthread_cond_init(&_M_cond, NULL);

    if (__e)
      __throw_system_error(__e);
#endif
  }
  
  condition_variable_any::~condition_variable_any()
  {
    __gthread_cond_destroy(&_M_cond);
  } 
}

#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1
