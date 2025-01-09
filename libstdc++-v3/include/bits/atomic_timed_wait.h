// -*- C++ -*- header.

// Copyright (C) 2020-2025 Free Software Foundation, Inc.
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

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/atomic_wait.h>

#if __glibcxx_atomic_wait
#include <bits/functional_hash.h>
#include <bits/this_thread_sleep.h>
#include <bits/chrono.h>

#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
#include <sys/time.h>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  namespace __detail
  {
    using __wait_clock_t = chrono::steady_clock;

    template<typename _Clock, typename _Dur>
      __wait_clock_t::time_point
      __to_wait_clock(const chrono::time_point<_Clock, _Dur>& __atime) noexcept
      {
	const typename _Clock::time_point __c_entry = _Clock::now();
	const __wait_clock_t::time_point __w_entry = __wait_clock_t::now();
	const auto __delta = __atime - __c_entry;
	using __w_dur = typename __wait_clock_t::duration;
	return __w_entry + chrono::ceil<__w_dur>(__delta);
      }

    template<typename _Dur>
      __wait_clock_t::time_point
      __to_wait_clock(const chrono::time_point<__wait_clock_t,
					       _Dur>& __atime) noexcept
      {
	using __w_dur = typename __wait_clock_t::duration;
	return chrono::ceil<__w_dur>(__atime);
      }

#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
#define _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT
    // returns true if wait ended before timeout
    bool
    __platform_wait_until(const __platform_wait_t* __addr,
			  __platform_wait_t __old,
			  const __wait_clock_t::time_point& __atime) noexcept
    {
      auto __s = chrono::time_point_cast<chrono::seconds>(__atime);
      auto __ns = chrono::duration_cast<chrono::nanoseconds>(__atime - __s);

      struct timespec __rt =
	{
	  static_cast<std::time_t>(__s.time_since_epoch().count()),
	  static_cast<long>(__ns.count())
	};

      auto __e = syscall (SYS_futex, __addr,
			  static_cast<int>(__futex_wait_flags::__wait_bitset_private),
			  __old, &__rt, nullptr,
			  static_cast<int>(__futex_wait_flags::__bitset_match_any));
      if (__e)
	{
	  if (errno == ETIMEDOUT)
	    return false;
	  if (errno != EINTR && errno != EAGAIN)
	    __throw_system_error(errno);
	}
      return true;
    }
#else
// define _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT and implement __platform_wait_until()
// if there is a more efficient primitive supported by the platform
// (e.g. __ulock_wait())which is better than pthread_cond_clockwait
#endif // ! PLATFORM_TIMED_WAIT

#ifdef _GLIBCXX_HAS_GTHREADS
    // Returns true if wait ended before timeout.
    bool
    __cond_wait_until(__condvar& __cv, mutex& __mx,
		      const __wait_clock_t::time_point& __atime)
    {
      auto __s = chrono::time_point_cast<chrono::seconds>(__atime);
      auto __ns = chrono::duration_cast<chrono::nanoseconds>(__atime - __s);

      __gthread_time_t __ts =
	{
	  static_cast<std::time_t>(__s.time_since_epoch().count()),
	  static_cast<long>(__ns.count())
	};

#ifdef _GLIBCXX_USE_PTHREAD_COND_CLOCKWAIT
      if constexpr (is_same_v<chrono::steady_clock, __wait_clock_t>)
	__cv.wait_until(__mx, CLOCK_MONOTONIC, __ts);
      else
#endif
	__cv.wait_until(__mx, __ts);
      return __wait_clock_t::now() < __atime;
    }
#endif // _GLIBCXX_HAS_GTHREADS

    inline __wait_result_type
    __spin_until_impl(const __platform_wait_t* __addr,
		      const __wait_args_base* __a,
		      const __wait_clock_t::time_point& __deadline)
    {
      __wait_args __args{ *__a };
      auto __t0 = __wait_clock_t::now();
      using namespace literals::chrono_literals;

      __platform_wait_t __val;
      auto __now = __wait_clock_t::now();
      for (; __now < __deadline; __now = __wait_clock_t::now())
	{
	  auto __elapsed = __now - __t0;
#ifndef _GLIBCXX_NO_SLEEP
	  if (__elapsed > 128ms)
	    this_thread::sleep_for(64ms);
	  else if (__elapsed > 64us)
	    this_thread::sleep_for(__elapsed / 2);
	  else
#endif
	  if (__elapsed > 4us)
	    __thread_yield();
	  else
	    {
	      auto __res = __detail::__spin_impl(__addr, __a);
	      if (__res.first)
		return __res;
	    }

	  __atomic_load(__addr, &__val, __args._M_order);
	  if (__val != __args._M_old)
	      return make_pair(true, __val);
	}
      return make_pair(false, __val);
    }

    inline __wait_result_type
    __wait_until_impl(const __platform_wait_t* __addr,
		      const __wait_args_base* __a,
		      const __wait_clock_t::time_point& __atime)
    {
      __wait_args __args{ *__a };
#ifdef _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT
      __waiter_pool_impl* __pool = nullptr;
#else
      // if we don't have __platform_wait, we always need the side-table
      __waiter_pool_impl* __pool = &__waiter_pool_impl::_S_impl_for(__addr);
#endif

      __platform_wait_t* __wait_addr;
      if (__args & __wait_flags::__proxy_wait)
	{
#ifdef _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT
	  __pool = &__waiter_pool_impl::_S_impl_for(__addr);
#endif
	  __wait_addr = &__pool->_M_ver;
	  __atomic_load(__wait_addr, &__args._M_old, __args._M_order);
	}
      else
	__wait_addr = const_cast<__platform_wait_t*>(__addr);

      if (__args & __wait_flags::__do_spin)
	{
	  auto __res = __detail::__spin_until_impl(__wait_addr, __a, __atime);
	  if (__res.first)
	    return __res;
	  if (__args & __wait_flags::__spin_only)
	    return __res;
	}

      if (!(__args & __wait_flags::__track_contention))
	{
	  // caller does not externally track contention
#ifdef _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT
	  __pool = (__pool == nullptr) ? &__waiter_pool_impl::_S_impl_for(__addr)
				       : __pool;
#endif
	  __pool->_M_enter_wait();
	}

      __wait_result_type __res;
#ifdef _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT
      if (__platform_wait_until(__wait_addr, __args._M_old, __atime))
	__res = make_pair(true, __args._M_old);
      else
	__res = make_pair(false, __args._M_old);
#else
      __platform_wait_t __val;
      __atomic_load(__wait_addr, &__val, __args._M_order);
      if (__val == __args._M_old)
	{
	  lock_guard<mutex> __l{ __pool->_M_mtx };
	  __atomic_load(__wait_addr, &__val, __args._M_order);
	  if (__val == __args._M_old &&
	      __cond_wait_until(__pool->_M_cv, __pool->_M_mtx, __atime))
	    __res = make_pair(true, __val);
	}
      else
	__res = make_pair(false, __val);
#endif

      if (!(__args & __wait_flags::__track_contention))
	// caller does not externally track contention
	__pool->_M_leave_wait();
      return __res;
    }

    template<typename _Clock, typename _Dur>
      __wait_result_type
      __wait_until(const __platform_wait_t* __addr, const __wait_args* __args,
		   const chrono::time_point<_Clock, _Dur>& __atime) noexcept
      {
	if constexpr (is_same_v<__wait_clock_t, _Clock>)
	  return __detail::__wait_until_impl(__addr, __args, __atime);
	else
	  {
	    auto __res = __detail::__wait_until_impl(__addr, __args,
						     __to_wait_clock(__atime));
	    if (!__res.first)
	      {
		// We got a timeout when measured against __clock_t but
		// we need to check against the caller-supplied clock
		// to tell whether we should return a timeout.
		if (_Clock::now() < __atime)
		  return make_pair(true, __res.second);
	      }
	    return __res;
	  }
      }

    template<typename _Rep, typename _Period>
      __wait_result_type
      __wait_for(const __platform_wait_t* __addr, const __wait_args_base* __a,
		 const chrono::duration<_Rep, _Period>& __rtime) noexcept
      {
	__wait_args __args{ *__a };
	if (!__rtime.count())
	  {
	    // no rtime supplied, just spin a bit
	    __args |= __wait_flags::__spin_only;
	    return __detail::__wait_impl(__addr, &__args);
	  }

	auto const __reltime = chrono::ceil<__wait_clock_t::duration>(__rtime);
	auto const __atime = chrono::steady_clock::now() + __reltime;
	return __detail::__wait_until(__addr, &__args, __atime);
      }
  } // namespace __detail

  // returns true if wait ended before timeout
  template<typename _Tp,
	   typename _Pred, typename _ValFn,
	   typename _Clock, typename _Dur>
    bool
    __atomic_wait_address_until(const _Tp* __addr, _Pred&& __pred,
				_ValFn&& __vfn,
				const chrono::time_point<_Clock, _Dur>& __atime,
				bool __bare_wait = false) noexcept
    {
      const auto __wait_addr =
	reinterpret_cast<const __detail::__platform_wait_t*>(__addr);
      __detail::__wait_args __args{ __addr, __bare_wait };
      _Tp __val = __vfn();
      while (!__pred(__val))
	{
	  auto __res = __detail::__wait_until(__wait_addr, &__args, __atime);
	  if (!__res.first)
	    // timed out
	    return __res.first; // C++26 will also return last observed __val
	  __val = __vfn();
	}
      return true; // C++26 will also return last observed __val
    }

  template<typename _Clock, typename _Dur>
    bool
    __atomic_wait_address_until_v(const __detail::__platform_wait_t* __addr,
				  __detail::__platform_wait_t __old,
				  int __order,
				  const chrono::time_point<_Clock, _Dur>& __atime,
				  bool __bare_wait = false) noexcept
    {
      __detail::__wait_args __args{ __addr, __old, __order, __bare_wait };
      auto __res = __detail::__wait_until(__addr, &__args, __atime);
      return __res.first; // C++26 will also return last observed __val
    }

  template<typename _Tp, typename _ValFn,
	   typename _Clock, typename _Dur>
    bool
    __atomic_wait_address_until_v(const _Tp* __addr, _Tp&& __old, _ValFn&& __vfn,
				  const chrono::time_point<_Clock, _Dur>& __atime,
				  bool __bare_wait = false) noexcept
    {
      auto __pfn = [&](const _Tp& __val)
	  { return !__detail::__atomic_compare(__old, __val); };
      return __atomic_wait_address_until(__addr, __pfn, forward<_ValFn>(__vfn),
					 __atime, __bare_wait);
    }

  template<typename _Tp,
	   typename _Pred, typename _ValFn,
	   typename _Rep, typename _Period>
    bool
    __atomic_wait_address_for(const _Tp* __addr, _Pred&& __pred,
			      _ValFn&& __vfn,
			      const chrono::duration<_Rep, _Period>& __rtime,
			      bool __bare_wait = false) noexcept
    {
      const auto __wait_addr =
	  reinterpret_cast<const __detail::__platform_wait_t*>(__addr);
      __detail::__wait_args __args{ __addr, __bare_wait };
      _Tp __val = __vfn();
      while (!__pred(__val))
	{
	  auto __res = __detail::__wait_for(__wait_addr, &__args, __rtime);
	  if (!__res.first)
	    // timed out
	    return __res.first; // C++26 will also return last observed __val
	  __val = __vfn();
	}
      return true; // C++26 will also return last observed __val
    }

  template<typename _Rep, typename _Period>
    bool
    __atomic_wait_address_for_v(const __detail::__platform_wait_t* __addr,
				__detail::__platform_wait_t __old,
				int __order,
				const chrono::time_point<_Rep, _Period>& __rtime,
				bool __bare_wait = false) noexcept
    {
      __detail::__wait_args __args{ __addr, __old, __order, __bare_wait };
      auto __res = __detail::__wait_for(__addr, &__args, __rtime);
      return __res.first; // C++26 will also return last observed __Val
    }

  template<typename _Tp, typename _ValFn,
	   typename _Rep, typename _Period>
    bool
    __atomic_wait_address_for_v(const _Tp* __addr, _Tp&& __old, _ValFn&& __vfn,
				const chrono::duration<_Rep, _Period>& __rtime,
				bool __bare_wait = false) noexcept
    {
      auto __pfn = [&](const _Tp& __val)
	  { return !__detail::__atomic_compare(__old, __val); };
      return __atomic_wait_address_for(__addr, __pfn, forward<_ValFn>(__vfn),
				       __rtime, __bare_wait);
    }
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // __cpp_lib_atomic_wait
#endif // _GLIBCXX_ATOMIC_TIMED_WAIT_H
