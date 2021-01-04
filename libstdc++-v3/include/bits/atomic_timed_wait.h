// -*- C++ -*- header.

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

/** @file bits/atomic_timed_wait.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{atomic}
 */

#ifndef _GLIBCXX_ATOMIC_TIMED_WAIT_H
#define _GLIBCXX_ATOMIC_TIMED_WAIT_H 1

#pragma GCC system_header

#include <bits/atomic_wait.h>

#if __cpp_lib_atomic_wait
#include <bits/functional_hash.h>

#include <chrono>

#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
#include <exception> // std::terminate
#include <sys/time.h>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  enum class __atomic_wait_status { no_timeout, timeout };

  namespace __detail
  {
#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
    using __platform_wait_clock_t = chrono::steady_clock;

    template<typename _Duration>
      __atomic_wait_status
      __platform_wait_until_impl(__platform_wait_t* __addr,
				 __platform_wait_t __val,
				 const chrono::time_point<
					  __platform_wait_clock_t, _Duration>&
				      __atime) noexcept
      {
	auto __s = chrono::time_point_cast<chrono::seconds>(__atime);
	auto __ns = chrono::duration_cast<chrono::nanoseconds>(__atime - __s);

	struct timespec __rt =
	{
	  static_cast<std::time_t>(__s.time_since_epoch().count()),
	  static_cast<long>(__ns.count())
	};

	auto __e = syscall (SYS_futex, __addr,
			    static_cast<int>(__futex_wait_flags::
						__wait_bitset_private),
			    __val, &__rt, nullptr,
			    static_cast<int>(__futex_wait_flags::
						__bitset_match_any));
	if (__e && !(errno == EINTR || errno == EAGAIN || errno == ETIMEDOUT))
	    std::terminate();
	return (__platform_wait_clock_t::now() < __atime)
	       ? __atomic_wait_status::no_timeout
	       : __atomic_wait_status::timeout;
      }

    template<typename _Clock, typename _Duration>
      __atomic_wait_status
      __platform_wait_until(__platform_wait_t* __addr, __platform_wait_t __val,
			    const chrono::time_point<_Clock, _Duration>&
				__atime)
      {
	if constexpr (is_same_v<__platform_wait_clock_t, _Clock>)
	  {
	    return __detail::__platform_wait_until_impl(__addr, __val, __atime);
	  }
	else
	  {
	    const typename _Clock::time_point __c_entry = _Clock::now();
	    const __platform_wait_clock_t::time_point __s_entry =
		    __platform_wait_clock_t::now();
	    const auto __delta = __atime - __c_entry;
	    const auto __s_atime = __s_entry + __delta;
	    if (__detail::__platform_wait_until_impl(__addr, __val, __s_atime)
		  == __atomic_wait_status::no_timeout)
	      return __atomic_wait_status::no_timeout;

	    // We got a timeout when measured against __clock_t but
	    // we need to check against the caller-supplied clock
	    // to tell whether we should return a timeout.
	    if (_Clock::now() < __atime)
	      return __atomic_wait_status::no_timeout;
	    return __atomic_wait_status::timeout;
	  }
      }
#else // ! FUTEX

#ifdef _GLIBCXX_USE_PTHREAD_COND_CLOCKWAIT
    template<typename _Duration>
      __atomic_wait_status
      __cond_wait_until_impl(__condvar& __cv, mutex& __mx,
	  const chrono::time_point<chrono::steady_clock, _Duration>& __atime)
      {
	auto __s = chrono::time_point_cast<chrono::seconds>(__atime);
	auto __ns = chrono::duration_cast<chrono::nanoseconds>(__atime - __s);

	__gthread_time_t __ts =
	  {
	    static_cast<std::time_t>(__s.time_since_epoch().count()),
	    static_cast<long>(__ns.count())
	  };

	__cv.wait_until(__mx, CLOCK_MONOTONIC, __ts);

	return (chrono::steady_clock::now() < __atime)
	       ? __atomic_wait_status::no_timeout
	       : __atomic_wait_status::timeout;
      }
#endif

    template<typename _Duration>
      __atomic_wait_status
      __cond_wait_until_impl(__condvar& __cv, mutex& __mx,
	  const chrono::time_point<chrono::system_clock, _Duration>& __atime)
      {
	auto __s = chrono::time_point_cast<chrono::seconds>(__atime);
	auto __ns = chrono::duration_cast<chrono::nanoseconds>(__atime - __s);

	__gthread_time_t __ts =
	{
	  static_cast<std::time_t>(__s.time_since_epoch().count()),
	  static_cast<long>(__ns.count())
	};

	__cv.wait_until(__mx, __ts);

	return (chrono::system_clock::now() < __atime)
	       ? __atomic_wait_status::no_timeout
	       : __atomic_wait_status::timeout;
      }

    // return true if timeout
    template<typename _Clock, typename _Duration>
      __atomic_wait_status
      __cond_wait_until(__condvar& __cv, mutex& __mx,
	  const chrono::time_point<_Clock, _Duration>& __atime)
      {
#ifndef _GLIBCXX_USE_PTHREAD_COND_CLOCKWAIT
	using __clock_t = chrono::system_clock;
#else
	using __clock_t = chrono::steady_clock;
	if constexpr (is_same_v<_Clock, chrono::steady_clock>)
	  return __detail::__cond_wait_until_impl(__cv, __mx, __atime);
	else
#endif
	if constexpr (is_same_v<_Clock, chrono::system_clock>)
	  return __detail::__cond_wait_until_impl(__cv, __mx, __atime);
	else
	  {
	    const typename _Clock::time_point __c_entry = _Clock::now();
	    const __clock_t::time_point __s_entry = __clock_t::now();
	    const auto __delta = __atime - __c_entry;
	    const auto __s_atime = __s_entry + __delta;
	    if (__detail::__cond_wait_until_impl(__cv, __mx, __s_atime)
		== __atomic_wait_status::no_timeout)
	      return __atomic_wait_status::no_timeout;
	    // We got a timeout when measured against __clock_t but
	    // we need to check against the caller-supplied clock
	    // to tell whether we should return a timeout.
	    if (_Clock::now() < __atime)
	      return __atomic_wait_status::no_timeout;
	    return __atomic_wait_status::timeout;
	  }
      }
#endif // FUTEX

    struct __timed_waiters : __waiters
    {
      template<typename _Clock, typename _Duration>
	__atomic_wait_status
	_M_do_wait_until(__platform_wait_t __version,
			 const chrono::time_point<_Clock, _Duration>& __atime)
	{
#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
	  return __detail::__platform_wait_until(&_M_ver, __version, __atime);
#else
	  __platform_wait_t __cur = 0;
	  __waiters::__lock_t __l(_M_mtx);
	  while (__cur <= __version)
	    {
	      if (__detail::__cond_wait_until(_M_cv, _M_mtx, __atime)
		    == __atomic_wait_status::timeout)
		return __atomic_wait_status::timeout;

	      __platform_wait_t __last = __cur;
	      __atomic_load(&_M_ver, &__cur, __ATOMIC_ACQUIRE);
	      if (__cur < __last)
		break; // break the loop if version overflows
	    }
	  return __atomic_wait_status::no_timeout;
#endif
	}

      static __timed_waiters&
      _S_timed_for(void* __t)
      {
	static_assert(sizeof(__timed_waiters) == sizeof(__waiters));
	return static_cast<__timed_waiters&>(__waiters::_S_for(__t));
      }
    };
  } // namespace __detail

  template<typename _Tp, typename _Pred,
	   typename _Clock, typename _Duration>
    bool
    __atomic_wait_until(const _Tp* __addr, _Tp __old, _Pred __pred,
			const chrono::time_point<_Clock, _Duration>&
			    __atime) noexcept
    {
      using namespace __detail;

      if (std::__atomic_spin(__pred))
	return true;

      auto& __w = __timed_waiters::_S_timed_for((void*)__addr);
      auto __version = __w._M_enter_wait();
      do
	{
	  __atomic_wait_status __res;
#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
	  if constexpr (__platform_wait_uses_type<_Tp>)
	    {
	      __res = __detail::__platform_wait_until((__platform_wait_t*)(void*) __addr,
						      __old, __atime);
	    }
	  else
#endif
	    {
	      __res = __w._M_do_wait_until(__version, __atime);
	    }
	  if (__res == __atomic_wait_status::timeout)
	    return false;
	}
      while (!__pred() && __atime < _Clock::now());
      __w._M_leave_wait();

      // if timed out, return false
      return (_Clock::now() < __atime);
    }

  template<typename _Tp, typename _Pred,
	   typename _Rep, typename _Period>
    bool
    __atomic_wait_for(const _Tp* __addr, _Tp __old, _Pred __pred,
		      const chrono::duration<_Rep, _Period>& __rtime) noexcept
    {
      using namespace __detail;

      if (std::__atomic_spin(__pred))
	return true;

      if (!__rtime.count())
	return false; // no rtime supplied, and spin did not acquire

      using __dur = chrono::steady_clock::duration;
      auto __reltime = chrono::duration_cast<__dur>(__rtime);
      if (__reltime < __rtime)
	++__reltime;

      return __atomic_wait_until(__addr, __old, std::move(__pred),
				 chrono::steady_clock::now() + __reltime);
    }
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // __cpp_lib_atomic_wait
#endif // _GLIBCXX_ATOMIC_TIMED_WAIT_H
