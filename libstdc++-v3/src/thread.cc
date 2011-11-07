// thread -*- C++ -*-

// Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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


#include <thread>
#include <system_error>
#include <cerrno>

#if defined(_GLIBCXX_USE_GET_NPROCS)
# include <sys/sysinfo.h>
# define _GLIBCXX_NPROCS get_nprocs()
#elif defined(_GLIBCXX_USE_PTHREADS_NUM_PROCESSORS_NP)
# define _GLIBCXX_NPROCS pthread_num_processors_np()
#elif defined(_GLIBCXX_USE_SYSCTL_HW_NCPU)
# include <stddef.h>
# include <sys/sysctl.h>
static inline int get_nprocs()
{
 int count;
 size_t size = sizeof(count);
 int mib[] = { CTL_HW, HW_NCPU };
 if (!sysctl(mib, 2, &count, &size, NULL, 0))
   return count;
 return 0;
}
# define _GLIBCXX_NPROCS get_nprocs()
#elif defined(_GLIBCXX_USE_SC_NPROCESSORS_ONLN)
# include <unistd.h>
# define _GLIBCXX_NPROCS sysconf(_SC_NPROCESSORS_ONLN)
#elif defined(_GLIBCXX_USE_SC_NPROC_ONLN)
# include <unistd.h>
# define _GLIBCXX_NPROCS sysconf(_SC_NPROC_ONLN)
#else
# define _GLIBCXX_NPROCS 0
#endif

#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)

namespace std _GLIBCXX_VISIBILITY(default)
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

_GLIBCXX_BEGIN_NAMESPACE_VERSION

  void
  thread::join()
  {
    int __e = EINVAL;

    if (_M_id != id())
      __e = __gthread_join(_M_id._M_thread, 0);

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
    if (!__gthread_active_p())
      __throw_system_error(int(errc::operation_not_permitted));

    __b->_M_this_ptr = __b;
    int __e = __gthread_create(&_M_id._M_thread,
			       &execute_native_thread_routine, __b.get());
    if (__e)
    {
      __b->_M_this_ptr.reset();
      __throw_system_error(__e);
    }
  }

  unsigned int
  thread::hardware_concurrency() noexcept
  {
    int __n = _GLIBCXX_NPROCS;
    if (__n < 0)
      __n = 0;
    return __n;
  }

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // _GLIBCXX_HAS_GTHREADS && _GLIBCXX_USE_C99_STDINT_TR1
