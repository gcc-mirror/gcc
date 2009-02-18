// thread -*- C++ -*-

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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

#include <thread>
#include <cerrno>

#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)

namespace std
{
  namespace
  {
    extern "C" void*
    execute_native_thread_routine(void* __p)
    {
      thread::_Impl_base* __t = static_cast<thread::_Impl_base*>(__p);
      thread::__shared_base_type __local;
      __local.swap(__t->_M_this_ptr);

      __try
	{
	  __t->_M_run();
	}
      __catch(...)
	{
	  std::terminate();
	}

      return 0;
    }
  }

  void
  thread::join()
  {
    int __e = EINVAL;

    if (_M_id != id())
      __e = __gthread_join(_M_id._M_thread, NULL);

    if (__e)
      __throw_system_error(__e);

    _M_id = id();
  }

  void
  thread::detach()
  {
    int __e = EINVAL;

    if (_M_id != id())
      __e = __gthread_detach(_M_id._M_thread);

    if (__e)
      __throw_system_error(__e);

    _M_id = id();
  }

  void
  thread::_M_start_thread(__shared_base_type __b)
  {
    __b->_M_this_ptr = __b;
    int __e = __gthread_create(&_M_id._M_thread,
			       &execute_native_thread_routine, __b.get());
    if (__e)
    {
      __b->_M_this_ptr.reset();
      __throw_system_error(__e);
    }
  }
}

#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1
