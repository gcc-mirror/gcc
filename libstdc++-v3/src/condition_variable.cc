// mutex -*- C++ -*-

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

namespace std
{
  condition_variable::condition_variable()
  {
#if __GTHREAD_HAS_COND
# if defined __GTHREAD_COND_INIT
    native_handle_type __tmp = __GTHREAD_COND_INIT;
    _M_cond = __tmp;
# else
    int __e = __gthread_cond_init(&_M_cond, NULL);
    if ( __e)
      __throw_system_error(__e);
# endif
#endif
  }

  condition_variable::~condition_variable()
  {
#if __GTHREAD_HAS_COND
    // XXX no thread blocked
    /* int __e = */ pthread_cond_destroy(&_M_cond);
    // if __e == EBUSY then blocked
#endif
  }

  void 
  condition_variable::notify_one()
  { 
#if __GTHREAD_HAS_COND
    int __e = pthread_cond_signal(&_M_cond);

    // XXX not in spec
    // EINVAL
    if ( __e)
      __throw_system_error(__e);
#endif
  }

  void 
  condition_variable::notify_all()
  { 
#if __GTHREAD_HAS_COND
    int __e = pthread_cond_broadcast(&_M_cond);

    // XXX not in spec
    // EINVAL
    if ( __e)
      __throw_system_error(__e);
#endif
  }

}

