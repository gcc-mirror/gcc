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

/** @file bits/atomic_wait.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{atomic}
 */

#ifndef _GLIBCXX_ATOMIC_WAIT_H
#define _GLIBCXX_ATOMIC_WAIT_H 1

#pragma GCC system_header

#include <bits/c++config.h>
#if defined _GLIBCXX_HAS_GTHREADS || defined _GLIBCXX_HAVE_LINUX_FUTEX
#include <bits/functional_hash.h>
#include <bits/gthr.h>
#include <ext/numeric_traits.h>

#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
# include <cerrno>
# include <climits>
# include <unistd.h>
# include <syscall.h>
# include <bits/functexcept.h>
// TODO get this from Autoconf
# define _GLIBCXX_HAVE_LINUX_FUTEX_PRIVATE 1
#else
# include <bits/std_mutex.h>  // std::mutex, std::__condvar
#endif

#define __cpp_lib_atomic_wait 201907L

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  namespace __detail
  {
    using __platform_wait_t = int;

    constexpr auto __atomic_spin_count_1 = 16;
    constexpr auto __atomic_spin_count_2 = 12;

    template<typename _Tp>
      inline constexpr bool __platform_wait_uses_type
#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
	= is_same_v<remove_cv_t<_Tp>, __platform_wait_t>;
#else
	= false;
#endif

#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
    enum class __futex_wait_flags : int
    {
#ifdef _GLIBCXX_HAVE_LINUX_FUTEX_PRIVATE
      __private_flag = 128,
#else
      __private_flag = 0,
#endif
      __wait = 0,
      __wake = 1,
      __wait_bitset = 9,
      __wake_bitset = 10,
      __wait_private = __wait | __private_flag,
      __wake_private = __wake | __private_flag,
      __wait_bitset_private = __wait_bitset | __private_flag,
      __wake_bitset_private = __wake_bitset | __private_flag,
      __bitset_match_any = -1
    };

    template<typename _Tp>
      void
      __platform_wait(const _Tp* __addr, __platform_wait_t __val) noexcept
      {
	for(;;)
	  {
	    auto __e = syscall (SYS_futex, static_cast<const void*>(__addr),
				  static_cast<int>(__futex_wait_flags::__wait_private),
				    __val, nullptr);
	    if (!__e || errno == EAGAIN)
	      break;
	    else if (errno != EINTR)
	      __throw_system_error(__e);
	  }
      }

    template<typename _Tp>
      void
      __platform_notify(const _Tp* __addr, bool __all) noexcept
      {
	syscall (SYS_futex, static_cast<const void*>(__addr),
		  static_cast<int>(__futex_wait_flags::__wake_private),
		    __all ? INT_MAX : 1);
      }
#endif

    struct __waiters
    {
      alignas(64) __platform_wait_t _M_ver = 0;
      alignas(64) __platform_wait_t _M_wait = 0;

#ifndef _GLIBCXX_HAVE_LINUX_FUTEX
      using __lock_t = lock_guard<mutex>;
      mutex _M_mtx;
      __condvar _M_cv;

      __waiters() noexcept = default;
#endif

      __platform_wait_t
      _M_enter_wait() noexcept
      {
	__platform_wait_t __res;
	__atomic_load(&_M_ver, &__res, __ATOMIC_ACQUIRE);
	__atomic_fetch_add(&_M_wait, 1, __ATOMIC_ACQ_REL);
	return __res;
      }

      void
      _M_leave_wait() noexcept
      {
	__atomic_fetch_sub(&_M_wait, 1, __ATOMIC_ACQ_REL);
      }

      void
      _M_do_wait(__platform_wait_t __version) noexcept
      {
#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
	__platform_wait(&_M_ver, __version);
#else
	__platform_wait_t __cur = 0;
	while (__cur <= __version)
	  {
	    __waiters::__lock_t __l(_M_mtx);
	    _M_cv.wait(_M_mtx);
	    __platform_wait_t __last = __cur;
	    __atomic_load(&_M_ver, &__cur, __ATOMIC_ACQUIRE);
	    if (__cur < __last)
	      break; // break the loop if version overflows
	  }
#endif
      }

      bool
      _M_waiting() const noexcept
      {
	__platform_wait_t __res;
	__atomic_load(&_M_wait, &__res, __ATOMIC_ACQUIRE);
	return __res;
      }

      void
      _M_notify(bool __all) noexcept
      {
	__atomic_fetch_add(&_M_ver, 1, __ATOMIC_ACQ_REL);
#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
	__platform_notify(&_M_ver, __all);
#else
	if (__all)
	  _M_cv.notify_all();
	else
	  _M_cv.notify_one();
#endif
      }

      static __waiters&
      _S_for(const void* __t)
      {
	const unsigned char __mask = 0xf;
	static __waiters __w[__mask + 1];

	auto __key = _Hash_impl::hash(__t) & __mask;
	return __w[__key];
      }
    };

    struct __waiter
    {
      __waiters& _M_w;
      __platform_wait_t _M_version;

      template<typename _Tp>
	__waiter(const _Tp* __addr) noexcept
	  : _M_w(__waiters::_S_for(static_cast<const void*>(__addr)))
	  , _M_version(_M_w._M_enter_wait())
	{ }

      ~__waiter()
      { _M_w._M_leave_wait(); }

      void _M_do_wait() noexcept
      { _M_w._M_do_wait(_M_version); }
    };

    inline void
    __thread_relax() noexcept
    {
#if defined __i386__ || defined __x86_64__
      __builtin_ia32_pause();
#elif defined _GLIBCXX_USE_SCHED_YIELD
      __gthread_yield();
#endif
    }

    inline void
    __thread_yield() noexcept
    {
#if defined _GLIBCXX_USE_SCHED_YIELD
     __gthread_yield();
#endif
    }

  } // namespace __detail

  template<typename _Pred>
    bool
    __atomic_spin(_Pred& __pred) noexcept
    {
      for (auto __i = 0; __i < __detail::__atomic_spin_count_1; ++__i)
	{
	  if (__pred())
	    return true;

	  if (__i < __detail::__atomic_spin_count_2)
	    __detail::__thread_relax();
	  else
	    __detail::__thread_yield();
	}
      return false;
    }

  template<typename _Tp, typename _Pred>
    void
    __atomic_wait(const _Tp* __addr, _Tp __old, _Pred __pred) noexcept
    {
      using namespace __detail;
      if (std::__atomic_spin(__pred))
	return;

      __waiter __w(__addr);
      while (!__pred())
	{
	  if constexpr (__platform_wait_uses_type<_Tp>)
	    {
	      __platform_wait(__addr, __old);
	    }
	  else
	    {
	      // TODO support timed backoff when this can be moved into the lib
	      __w._M_do_wait();
	    }
	}
    }

  template<typename _Tp>
    void
    __atomic_notify(const _Tp* __addr, bool __all) noexcept
    {
      using namespace __detail;
      auto& __w = __waiters::_S_for((void*)__addr);
      if (!__w._M_waiting())
	return;

#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
      if constexpr (__platform_wait_uses_type<_Tp>)
	{
	  __platform_notify((__platform_wait_t*)(void*) __addr, __all);
	}
      else
#endif
	{
	  __w._M_notify(__all);
	}
    }
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // GTHREADS || LINUX_FUTEX
#endif // _GLIBCXX_ATOMIC_WAIT_H
