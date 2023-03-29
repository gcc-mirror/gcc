// thread -*- C++ -*-

// Copyright (C) 2008-2023 Free Software Foundation, Inc.
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


#define _GLIBCXX_THREAD_ABI_COMPAT 1
#define _GLIBCXX_THREAD_IMPL 1
#include <memory> // include this first so <thread> can use shared_ptr
#include <thread>
#include <system_error>
#include <cerrno>
#include <cxxabi_forced.h>

#ifndef _GLIBCXX_USE_NANOSLEEP
# ifdef _GLIBCXX_HAVE_SLEEP
#  include <unistd.h>
# elif defined(_GLIBCXX_USE_WIN32_SLEEP)
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
# elif defined _GLIBCXX_NO_SLEEP && defined _GLIBCXX_HAS_GTHREADS
// We expect to be able to sleep for targets that support multiple threads:
#  error "No sleep function known for this target"
# endif
#endif

#ifdef _GLIBCXX_HAS_GTHREADS

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
#elif defined(_GLIBCXX_USE_GET_NPROCS_WIN32)
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
static inline int get_nprocs()
{
  SYSTEM_INFO sysinfo;
  GetSystemInfo (&sysinfo);
  return (int)sysinfo.dwNumberOfProcessors;
}
# define _GLIBCXX_NPROCS get_nprocs()
#elif defined(_GLIBCXX_USE_SC_NPROCESSORS_ONLN)
# include <unistd.h>
# define _GLIBCXX_NPROCS sysconf(_SC_NPROCESSORS_ONLN)
#elif defined(_GLIBCXX_USE_SC_NPROC_ONLN)
# include <unistd.h>
# define _GLIBCXX_NPROCS sysconf(_SC_NPROC_ONLN)
#elif defined(_WIN32)
# define WIN32_LEAN_AND_MEAN
# include <windows.h>
static inline int get_nprocs()
{
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  return (int) sysinfo.dwNumberOfProcessors;
}
# define _GLIBCXX_NPROCS get_nprocs()
#else
# define _GLIBCXX_NPROCS 0
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
  extern "C"
  {
    static void*
    execute_native_thread_routine(void* __p)
    {
      thread::_State_ptr __t{ static_cast<thread::_State*>(__p) };
      __t->_M_run();
      return nullptr;
    }

#if _GLIBCXX_THREAD_ABI_COMPAT
    static void*
    execute_native_thread_routine_compat(void* __p)
    {
      thread::_Impl_base* __t = static_cast<thread::_Impl_base*>(__p);
      thread::__shared_base_type __local;
      // Now that a new thread has been created we can transfer ownership of
      // the thread state to a local object, breaking the reference cycle
      // created in thread::_M_start_thread.
      __local.swap(__t->_M_this_ptr);
      __t->_M_run();
      return nullptr;
    }
#endif
  } // extern "C"

_GLIBCXX_BEGIN_NAMESPACE_VERSION

  thread::_State::~_State() = default;

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
  thread::_M_start_thread(_State_ptr state, void (*depend)())
  {
    // Make sure it's not optimized out, not even with LTO.
    asm ("" : : "rm" (depend));

    if (!__gthread_active_p())
      {
#if __cpp_exceptions
	throw system_error(make_error_code(errc::operation_not_permitted),
			   "Enable multithreading to use std::thread");
#else
	__builtin_abort();
#endif
      }

    const int err = __gthread_create(&_M_id._M_thread,
				     &execute_native_thread_routine,
				     state.get());
    if (err)
      __throw_system_error(err);
    state.release();
  }

#if _GLIBCXX_THREAD_ABI_COMPAT
  void
  thread::_M_start_thread(__shared_base_type __b)
  {
    if (!__gthread_active_p())
#if __cpp_exceptions
      throw system_error(make_error_code(errc::operation_not_permitted),
			 "Enable multithreading to use std::thread");
#else
      __throw_system_error(int(errc::operation_not_permitted));
#endif

    _M_start_thread(std::move(__b), nullptr);
  }

  void
  thread::_M_start_thread(__shared_base_type __b, void (*depend)())
  {
    // Make sure it's not optimized out, not even with LTO.
    asm ("" : : "rm" (depend));

    auto ptr = __b.get();
    // Create a reference cycle that will be broken in the new thread.
    ptr->_M_this_ptr = std::move(__b);
    int __e = __gthread_create(&_M_id._M_thread,
			       &execute_native_thread_routine_compat, ptr);
    if (__e)
    {
      ptr->_M_this_ptr.reset();  // break reference cycle, destroying *ptr.
      __throw_system_error(__e);
    }
  }
#endif

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

#endif // _GLIBCXX_HAS_GTHREADS

#ifndef _GLIBCXX_NO_SLEEP
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace this_thread
{
  void
  __sleep_for(chrono::seconds __s, chrono::nanoseconds __ns)
  {
#ifdef _GLIBCXX_USE_NANOSLEEP
    struct ::timespec __ts =
      {
	static_cast<std::time_t>(__s.count()),
	static_cast<long>(__ns.count())
      };
    while (::nanosleep(&__ts, &__ts) == -1 && errno == EINTR)
      { }
#elif defined(_GLIBCXX_HAVE_SLEEP)
    const auto target = chrono::steady_clock::now() + __s + __ns;
    while (true)
      {
	unsigned secs = __s.count();
	if (__ns.count() > 0)
	  {
# ifdef _GLIBCXX_HAVE_USLEEP
	    long us = __ns.count() / 1000;
	    if (us == 0)
	      us = 1;
	    ::usleep(us);
# else
	    if (__ns.count() > 1000000 || secs == 0)
	      ++secs; // No sub-second sleep function, so round up.
# endif
	  }

	if (secs > 0)
	  {
	    // Sleep in a loop to handle interruption by signals:
	    while ((secs = ::sleep(secs)))
	      { }
	  }
	const auto now = chrono::steady_clock::now();
	if (now >= target)
	  break;
	__s = chrono::duration_cast<chrono::seconds>(target - now);
	__ns = chrono::duration_cast<chrono::nanoseconds>(target - (now + __s));
    }
#elif defined(_GLIBCXX_USE_WIN32_SLEEP)
    unsigned long ms = __ns.count() / 1000000;
    if (__ns.count() > 0 && ms == 0)
      ms = 1;
    ::Sleep(chrono::milliseconds(__s).count() + ms);
#endif
  }
}
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // ! NO_SLEEP
