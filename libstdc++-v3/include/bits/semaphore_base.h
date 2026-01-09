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

/** @file bits/semaphore_base.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{semaphore}
 */

#ifndef _GLIBCXX_SEMAPHORE_BASE_H
#define _GLIBCXX_SEMAPHORE_BASE_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/version.h>

#ifdef __glibcxx_semaphore // C++ >= 20 && hosted && atomic_wait
#include <bits/atomic_base.h>
#include <bits/chrono.h>
#include <bits/atomic_timed_wait.h>
#include <ext/numeric_traits.h>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  struct __semaphore_impl
  {
    using __count_type = ptrdiff_t;

    static constexpr ptrdiff_t _S_max
      = __gnu_cxx::__int_traits<__count_type>::__max;

    constexpr explicit
    __semaphore_impl(__count_type __count) noexcept
    : _M_counter(__count)
    { }

    __semaphore_impl(const __semaphore_impl&) = delete;
    __semaphore_impl& operator=(const __semaphore_impl&) = delete;

    // Load the current counter value.
    _GLIBCXX_ALWAYS_INLINE __count_type
    _M_get_current() const noexcept
    { return __atomic_impl::load(&_M_counter, memory_order::acquire); }

    // Try to acquire the semaphore (i.e. decrement the counter).
    // Returns false if the current counter is zero, or if another thread
    // changes the value first. In the latter case, __cur is set to the new
    // value.
    _GLIBCXX_ALWAYS_INLINE bool
    _M_do_try_acquire(__count_type& __cur) noexcept
    {
      if (__cur == 0)
	return false; // Cannot decrement when it's already zero.

      return __atomic_impl::compare_exchange_strong(&_M_counter,
						    __cur, __cur - 1,
						    memory_order::acquire,
						    memory_order::relaxed);
    }

    // Keep trying to acquire the semaphore in a loop until it succeeds.
    void
    _M_acquire() noexcept
    {
      auto __vfn = [this]{ return _M_get_current(); };
      _Available __is_available{__vfn()};
      while (!_M_do_try_acquire(__is_available._M_val))
	if (!__is_available())
	  std::__atomic_wait_address(&_M_counter, __is_available, __vfn, true);
    }

    // Try to acquire the semaphore, retrying a small number of times
    // in case of contention.
    bool
    _M_try_acquire() noexcept
    {
      // The fastest implementation of this function is just _M_do_try_acquire
      // but that can fail under contention even when _M_count > 0.

      auto __vfn = [this]{ return _M_get_current(); };
      _Available __is_available{__vfn()};

      // Retry the compare exchange a few times in case of contention:
      for (int __i = 0; __i < 10 && __is_available(); ++__i)
	if (_M_do_try_acquire(__is_available._M_val))
	  return true;

      // Spinloop to see if it becomes available.
      constexpr auto __zero = __detail::__wait_clock_t::duration{};
      if (std::__atomic_wait_address_for(&_M_counter, __is_available,
					 __vfn, __zero, true))
	return false; // timed out

      // Should be available, try once more to acquire it:
      return _M_do_try_acquire(__is_available._M_val);
    }

    template<typename _Clock, typename _Duration>
      bool
      _M_try_acquire_until(const chrono::time_point<_Clock, _Duration>& __atime) noexcept
      {
	auto __vfn = [this]{ return _M_get_current(); };
	_Available __is_available{__vfn()};
	while (!_M_do_try_acquire(__is_available._M_val))
	  if (!__is_available())
	    if (!std::__atomic_wait_address_until(&_M_counter, __is_available,
						  __vfn, __atime, true))
	      return false; // timed out
	return true;
      }

    template<typename _Rep, typename _Period>
      bool
      _M_try_acquire_for(const chrono::duration<_Rep, _Period>& __rtime) noexcept
      {
	return _M_try_acquire_until(__detail::__wait_clock_t::now() + __rtime);
      }

    _GLIBCXX_ALWAYS_INLINE ptrdiff_t
    _M_release(ptrdiff_t __update) noexcept
    {
      auto __old = __atomic_impl::fetch_add(&_M_counter, __update,
					    memory_order::release);
      if (__old == 0 && __update > 0)
	__atomic_notify_address(&_M_counter, true, true);
      return __old;
    }

  private:
    struct _Available
    {
      __count_type _M_val; // Cache of the last value loaded from _M_counter.

      // Returns true if the cached value is non-zero and so it should be
      // possible to acquire the semaphore.
      bool operator()() const noexcept { return _M_val > 0; }

      // Argument should be the latest value of the counter.
      // Returns true (and caches the value) if it's non-zero, meaning it
      // should be possible to acquire the semaphore. Returns false otherwise.
      bool operator()(__count_type __cur) noexcept
      {
	if (__cur == 0)
	  return false;
	_M_val = __cur;
	return true;
      }
    };

    alignas(__atomic_ref<__count_type>::required_alignment)
      __count_type _M_counter;
  };

  // Optimized specialization using __platform_wait (if available)
  template<bool _Binary>
  struct __platform_semaphore_impl
  {
    using __count_type = __detail::__platform_wait_t;

    static consteval ptrdiff_t
    _S_calc_max()
    {
      if (_Binary)
	return 1;
      else if ((ptrdiff_t)__gnu_cxx::__int_traits<__count_type>::__max < 0)
	return __gnu_cxx::__int_traits<ptrdiff_t>::__max;
      else
	return __gnu_cxx::__int_traits<__count_type>::__max;
    }

    static constexpr ptrdiff_t _S_max = _S_calc_max();

    constexpr explicit
    __platform_semaphore_impl(__count_type __count) noexcept
    : _M_counter(__count)
    { }

    __platform_semaphore_impl(__platform_semaphore_impl&) = delete;
    __platform_semaphore_impl& operator=(const __platform_semaphore_impl&) = delete;

    // Load the current counter value.
    _GLIBCXX_ALWAYS_INLINE __count_type
    _M_get_current() const noexcept
    {
      if constexpr (_Binary)
	return 1; // Not necessarily true, but optimistically assume it is.
      else
	return __atomic_impl::load(&_M_counter, memory_order::acquire);
    }

    // Try to acquire the semaphore (i.e. decrement the counter).
    // Returns false if the current counter is zero, or if another thread
    // changes the value first. In the latter case, __cur is set to the new
    // value.
    _GLIBCXX_ALWAYS_INLINE bool
    _M_do_try_acquire(__count_type& __cur) noexcept
    {
      if (__cur == 0)
	return false; // Cannot decrement when it's already zero.

      return __atomic_impl::compare_exchange_strong(&_M_counter,
						    __cur, __cur - 1,
						    memory_order::acquire,
						    memory_order::relaxed);
    }

    // Keep trying to acquire the semaphore in a loop until it succeeds.
    void
    _M_acquire() noexcept
    {
      auto __val = _M_get_current();
      while (!_M_do_try_acquire(__val))
	if (__val == 0)
	  {
	    std::__atomic_wait_address_v(&_M_counter, __val, __ATOMIC_ACQUIRE,
					 true);
	    __val = _M_get_current();
	  }
    }

    // Try to acquire the semaphore.
    bool
    _M_try_acquire() noexcept
    {
      if constexpr (_Binary)
	{
	  __count_type __val = 1;
	  // Do not expect much contention on binary semaphore, only try once.
	  return _M_do_try_acquire(__val);
	}
      else
	// Fastest implementation of this function is just _M_do_try_acquire
	// but that can fail under contention even when _M_count > 0.
	// Using _M_try_acquire_for(0ns) will retry a few times in a loop.
	return _M_try_acquire_for(__detail::__wait_clock_t::duration{});
    }

    template<typename _Clock, typename _Duration>
      bool
      _M_try_acquire_until(const chrono::time_point<_Clock, _Duration>& __atime) noexcept
      {
	auto __val = _M_get_current();
	while (!_M_do_try_acquire(__val))
	  if (__val == 0)
	    {
	      if (!std::__atomic_wait_address_until_v(&_M_counter, 0,
						      __ATOMIC_ACQUIRE,
						      __atime, true))
		return false; // timed out
	      __val = _M_get_current();
	    }
	return true;
      }

    template<typename _Rep, typename _Period>
      bool
      _M_try_acquire_for(const chrono::duration<_Rep, _Period>& __rtime) noexcept
      {
	auto __val = _M_get_current();
	while (!_M_do_try_acquire(__val))
	  if (__val == 0)
	    {
	      if (!std::__atomic_wait_address_for_v(&_M_counter, 0,
						    __ATOMIC_ACQUIRE,
						    __rtime, true))
		return false; // timed out
	      __val = _M_get_current();
	    }
	return true;
      }

    _GLIBCXX_ALWAYS_INLINE ptrdiff_t
    _M_release(ptrdiff_t __update) noexcept
    {
      auto __old = __atomic_impl::fetch_add(&_M_counter, __update,
					    memory_order::release);
      if (__old == 0 && __update > 0)
	__atomic_notify_address(&_M_counter, true, true);
      return __old;
    }

  protected:
    alignas(__detail::__platform_wait_alignment) __count_type _M_counter;
  };

  template<ptrdiff_t _Max, typename _Tp = __detail::__platform_wait_t>
    using _Semaphore_impl
      = __conditional_t<__platform_wait_uses_type<_Tp>
			  && _Max <= __gnu_cxx::__int_traits<_Tp>::__max,
			__platform_semaphore_impl<(_Max <= 1)>,
			__semaphore_impl>;

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // __glibcxx_semaphore
#endif // _GLIBCXX_SEMAPHORE_BASE_H
