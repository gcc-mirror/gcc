// -*- C++ -*- header.

// Copyright (C) 2020-2026 Free Software Foundation, Inc.
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
	if constexpr (is_same_v<__w_dur, _Dur>)
	  return __atime;
	else
	  return chrono::ceil<__w_dur>(__atime);
      }

    // This uses a nanoseconds duration for the timeout argument.
    // For __abi_version=0 that is the time since the steady_clock's epoch.
    // It's possible that in future we will add new __wait_flags constants
    // to indicate that the timeout is the time since the system_clock epoch,
    // or is a relative timeout not an absolute time.
    __wait_result_type
    __wait_until_impl(const void* __addr, __wait_args_base& __args,
		      const chrono::nanoseconds& __timeout);

    template<typename _Clock, typename _Dur>
      __wait_result_type
      __wait_until(const void* __addr, __wait_args_base& __args,
		   const chrono::time_point<_Clock, _Dur>& __atime) noexcept
      {
	auto __at = __detail::__to_wait_clock(__atime);
	auto __res = __detail::__wait_until_impl(__addr, __args,
						 __at.time_since_epoch());

	if constexpr (!is_same_v<__wait_clock_t, _Clock>)
	  if (__res._M_timeout)
	    {
	      // We got a timeout when measured against __clock_t but
	      // we need to check against the caller-supplied clock
	      // to tell whether we should return a timeout.
	      if (_Clock::now() < __atime)
		__res._M_timeout = false;
	    }
	return __res;
      }

    template<typename _Rep, typename _Period>
      __wait_result_type
      __wait_for(const void* __addr, __wait_args_base& __args,
		 const chrono::duration<_Rep, _Period>& __rtime) noexcept
      {
	if (!__rtime.count())
	  {
	    // no rtime supplied, just spin a bit
	    __args._M_flags |= __wait_flags::__do_spin | __wait_flags::__spin_only;
	    return __detail::__wait_impl(__addr, __args);
	  }

	auto const __reltime = chrono::ceil<__wait_clock_t::duration>(__rtime);
	auto const __atime = chrono::steady_clock::now() + __reltime;
	return __detail::__wait_until(__addr, __args, __atime);
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
      __detail::__wait_args __args{ __addr, __bare_wait };
      _Tp __val = __args._M_setup_wait(__addr, __vfn);
      while (!__pred(__val))
	{
	  auto __res = __detail::__wait_until(__addr, __args, __atime);
	  if (__res._M_timeout)
	    return false; // C++26 will also return last observed __val
	  __val = __args._M_on_wake(__addr, __vfn, __res);
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
      // This function must not be used if __wait_impl might use a proxy wait:
      __glibcxx_assert(__platform_wait_uses_type<__detail::__platform_wait_t>);

      __detail::__wait_args __args{ __addr, __old, __order, __bare_wait };
      auto __res = __detail::__wait_until(__addr, __args, __atime);
      return !__res._M_timeout; // C++26 will also return last observed __val
    }

  template<typename _Tp, typename _ValFn,
	   typename _Clock, typename _Dur>
    bool
    __atomic_wait_address_until_v(const _Tp* __addr, _Tp&& __old,
				  _ValFn&& __vfn,
				  const chrono::time_point<_Clock, _Dur>& __atime,
				  bool __bare_wait = false) noexcept
    {
      auto __pfn = [&](const _Tp& __val) {
	return !__detail::__atomic_eq(__old, __val);
      };
      return std::__atomic_wait_address_until(__addr, __pfn, __vfn, __atime,
					      __bare_wait);
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
      __detail::__wait_args __args{ __addr, __bare_wait };
      _Tp __val = __args._M_setup_wait(__addr, __vfn);
      while (!__pred(__val))
	{
	  auto __res = __detail::__wait_for(__addr, __args, __rtime);
	  if (__res._M_timeout)
	    return false; // C++26 will also return last observed __val
	  __val = __args._M_on_wake(__addr, __vfn, __res);
	}
      return true; // C++26 will also return last observed __val
    }

  template<typename _Rep, typename _Period>
    bool
    __atomic_wait_address_for_v(const __detail::__platform_wait_t* __addr,
				__detail::__platform_wait_t __old,
				int __order,
				const chrono::duration<_Rep, _Period>& __rtime,
				bool __bare_wait = false) noexcept
    {
      // This function must not be used if __wait_impl might use a proxy wait:
      __glibcxx_assert(__platform_wait_uses_type<__detail::__platform_wait_t>);

      __detail::__wait_args __args{ __addr, __old, __order, __bare_wait };
      auto __res = __detail::__wait_for(__addr, __args, __rtime);
      return !__res._M_timeout; // C++26 will also return last observed __val
    }

  template<typename _Tp, typename _ValFn,
	   typename _Rep, typename _Period>
    bool
    __atomic_wait_address_for_v(const _Tp* __addr, _Tp&& __old, _ValFn&& __vfn,
				const chrono::duration<_Rep, _Period>& __rtime,
				bool __bare_wait = false) noexcept
    {
      auto __pfn = [&](const _Tp& __val) {
	return !__detail::__atomic_eq(__old, __val);
      };
      return __atomic_wait_address_for(__addr, __pfn, forward<_ValFn>(__vfn),
				       __rtime, __bare_wait);
    }
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // __cpp_lib_atomic_wait
#endif // _GLIBCXX_ATOMIC_TIMED_WAIT_H
