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

/** @file bits/atomic_wait.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{atomic}
 */

#ifndef _GLIBCXX_ATOMIC_WAIT_H
#define _GLIBCXX_ATOMIC_WAIT_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#include <bits/version.h>

#if __glibcxx_atomic_wait
#include <cstdint>
#include <bits/functional_hash.h>
#include <bits/gthr.h>
#include <ext/numeric_traits.h>

#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
# include <cerrno>
# include <climits>
# include <unistd.h>
# include <syscall.h>
# include <bits/functexcept.h>
#endif

#include <bits/stl_pair.h>
#include <bits/std_mutex.h>  // std::mutex, std::__condvar

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
  namespace __detail
  {
#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
#define _GLIBCXX_HAVE_PLATFORM_WAIT 1
    using __platform_wait_t = int;
    inline constexpr size_t __platform_wait_alignment = 4;
#else
// define _GLIBCX_HAVE_PLATFORM_WAIT and implement __platform_wait()
// and __platform_notify() if there is a more efficient primitive supported
// by the platform (e.g. __ulock_wait()/__ulock_wake()) which is better than
// a mutex/condvar based wait.
# if ATOMIC_LONG_LOCK_FREE == 2
    using __platform_wait_t = unsigned long;
# else
    using __platform_wait_t = unsigned int;
# endif
    inline constexpr size_t __platform_wait_alignment
      = __alignof__(__platform_wait_t);
#endif
  } // namespace __detail

  template<typename _Tp>
    inline constexpr bool __platform_wait_uses_type
#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
      = is_scalar_v<_Tp>
	&& ((sizeof(_Tp) == sizeof(__detail::__platform_wait_t))
	&& (alignof(_Tp*) >= __detail::__platform_wait_alignment));
#else
      = false;
#endif

  namespace __detail
  {
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
	auto __e = syscall (SYS_futex, static_cast<const void*>(__addr),
			    static_cast<int>(__futex_wait_flags::__wait_private),
			    __val, nullptr);
	if (!__e || errno == EAGAIN)
	  return;
	if (errno != EINTR)
	  __throw_system_error(errno);
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

    inline void
    __thread_yield() noexcept
    {
#if defined _GLIBCXX_HAS_GTHREADS && defined _GLIBCXX_USE_SCHED_YIELD
      __gthread_yield();
#endif
    }

    inline void
    __thread_relax() noexcept
    {
#if defined __i386__ || defined __x86_64__
      __builtin_ia32_pause();
#else
      __thread_yield();
#endif
    }

    inline constexpr auto __atomic_spin_count_relax = 12;
    inline constexpr auto __atomic_spin_count = 16;

    // return true if equal
    template<typename _Tp>
      bool __atomic_compare(const _Tp& __a, const _Tp& __b)
      {
	// TODO make this do the correct padding bit ignoring comparison
	return __builtin_memcmp(&__a, &__b, sizeof(_Tp)) == 0;
      }

    struct __waiter_pool_impl
    {
      // Don't use std::hardware_destructive_interference_size here because we
      // don't want the layout of library types to depend on compiler options.
      static constexpr auto _S_align = 64;

      alignas(_S_align) __platform_wait_t _M_wait = 0;

#ifndef _GLIBCXX_HAVE_PLATFORM_WAIT
      mutex _M_mtx;
#endif

      alignas(_S_align) __platform_wait_t _M_ver = 0;

#ifndef _GLIBCXX_HAVE_PLATFORM_WAIT
      __condvar _M_cv;
#endif
      __waiter_pool_impl() = default;

      void
      _M_enter_wait() noexcept
      { __atomic_fetch_add(&_M_wait, 1, __ATOMIC_SEQ_CST); }

      void
      _M_leave_wait() noexcept
      { __atomic_fetch_sub(&_M_wait, 1, __ATOMIC_RELEASE); }

      bool
      _M_waiting() const noexcept
      {
	__platform_wait_t __res;
	__atomic_load(&_M_wait, &__res, __ATOMIC_SEQ_CST);
	return __res != 0;
      }

      static __waiter_pool_impl&
      _S_impl_for(const void* __addr) noexcept
      {
	constexpr __UINTPTR_TYPE__ __ct = 16;
	static __waiter_pool_impl __w[__ct];
	auto __key = ((__UINTPTR_TYPE__)__addr >> 2) % __ct;
	return __w[__key];
      }
    };

    enum class __wait_flags : __UINT_LEAST32_TYPE__
    {
       __abi_version = 0,
       __proxy_wait = 1,
       __track_contention = 2,
       __do_spin = 4,
       __spin_only = 8 | __do_spin, // implies __do_spin
       __abi_version_mask = 0xffff0000,
    };

    struct __wait_args
    {
      __platform_wait_t _M_old;
      int _M_order = __ATOMIC_ACQUIRE;
      __wait_flags _M_flags;

      template<typename _Tp>
	explicit __wait_args(const _Tp* __addr,
			     bool __bare_wait = false) noexcept
	    : _M_flags{ _S_flags_for(__addr, __bare_wait) }
	{ }

      __wait_args(const __platform_wait_t* __addr, __platform_wait_t __old,
		  int __order, bool __bare_wait = false) noexcept
	  : _M_old{ __old }
	  , _M_order{ __order }
	  , _M_flags{ _S_flags_for(__addr, __bare_wait) }
	{ }

      __wait_args(const __wait_args&) noexcept = default;
      __wait_args&
      operator=(const __wait_args&) noexcept = default;

      bool
      operator&(__wait_flags __flag) const noexcept
      {
	 using __t = underlying_type_t<__wait_flags>;
	 return static_cast<__t>(_M_flags)
	     & static_cast<__t>(__flag);
      }

      __wait_args
      operator|(__wait_flags __flag) const noexcept
      {
	using __t = underlying_type_t<__wait_flags>;
	__wait_args __res{ *this };
	const auto __flags = static_cast<__t>(__res._M_flags)
			     | static_cast<__t>(__flag);
	__res._M_flags = __wait_flags{ __flags };
	return __res;
      }

    private:
      static int
      constexpr _S_default_flags() noexcept
      {
	using __t = underlying_type_t<__wait_flags>;
	return static_cast<__t>(__wait_flags::__abi_version)
		| static_cast<__t>(__wait_flags::__do_spin);
      }

      template<typename _Tp>
	static int
	constexpr _S_flags_for(const _Tp*, bool __bare_wait) noexcept
	{
	  auto __res = _S_default_flags();
	  if (!__bare_wait)
	    __res |= static_cast<int>(__wait_flags::__track_contention);
	  if constexpr (!__platform_wait_uses_type<_Tp>)
	    __res |= static_cast<int>(__wait_flags::__proxy_wait);
	  return __res;
	}

      template<typename _Tp>
	static int
	_S_memory_order_for(const _Tp*, int __order) noexcept
	{
	  if constexpr (__platform_wait_uses_type<_Tp>)
	    return __order;
	  return __ATOMIC_ACQUIRE;
	}
    };

    using __wait_result_type = pair<bool, __platform_wait_t>;
    inline __wait_result_type
    __spin_impl(const __platform_wait_t* __addr, __wait_args __args)
    {
      __platform_wait_t __val;
      for (auto __i = 0; __i < __atomic_spin_count; ++__i)
	{
	  __atomic_load(__addr, &__val, __args._M_order);
	  if (__val != __args._M_old)
	    return make_pair(true, __val);
	  if (__i < __atomic_spin_count_relax)
	    __detail::__thread_relax();
	  else
	    __detail::__thread_yield();
	}
      return make_pair(false, __val);
    }

    inline __wait_result_type
    __wait_impl(const __platform_wait_t* __addr, __wait_args __args)
    {
#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
      __waiter_pool_impl* __pool = nullptr;
#else
      // if we don't have __platform_wait, we always need the side-table
      __waiter_pool_impl* __pool = &__waiter_pool_impl::_S_impl_for(__addr);
#endif

      __platform_wait_t* __wait_addr;
      if (__args & __wait_flags::__proxy_wait)
	{
#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
	  __pool = &__waiter_pool_impl::_S_impl_for(__addr);
#endif
	  __wait_addr = &__pool->_M_ver;
	  __atomic_load(__wait_addr, &__args._M_old, __args._M_order);
	}
      else
	__wait_addr = const_cast<__platform_wait_t*>(__addr);

      if (__args & __wait_flags::__do_spin)
	{
	  auto __res = __detail::__spin_impl(__wait_addr, __args);
	  if (__res.first)
	    return __res;
	  if (__args & __wait_flags::__spin_only)
	    return __res;
	}

      if (!(__args & __wait_flags::__track_contention))
	{
	  // caller does not externally track contention
#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
	  __pool = (__pool == nullptr) ? &__waiter_pool_impl::_S_impl_for(__addr)
				       : __pool;
#endif
	  __pool->_M_enter_wait();
	}

      __wait_result_type __res;
#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
      __platform_wait(__wait_addr, __args._M_old);
      __res = make_pair(false, __args._M_old);
#else
      __platform_wait_t __val;
      __atomic_load(__wait_addr, &__val, __args._M_order);
      if (__val == __args._M_old)
	{
	    lock_guard<mutex> __l{ __pool->_M_mtx };
	    __atomic_load(__wait_addr, &__val, __args._M_order);
	    if (__val == __args._M_old)
		__pool->_M_cv.wait(__pool->_M_mtx);
	}
      __res = make_pair(false, __val);
#endif

      if (!(__args & __wait_flags::__track_contention))
	// caller does not externally track contention
	__pool->_M_leave_wait();
      return __res;
    }

    inline void
    __notify_impl(const __platform_wait_t* __addr, [[maybe_unused]] bool __all,
		  __wait_args __args)
    {
#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
      __waiter_pool_impl* __pool = nullptr;
#else
      // if we don't have __platform_notify, we always need the side-table
      __waiter_pool_impl* __pool = &__waiter_pool_impl::_S_impl_for(__addr);
#endif

      if (!(__args & __wait_flags::__track_contention))
	{
#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
	  __pool = &__waiter_pool_impl::_S_impl_for(__addr);
#endif
	  if (!__pool->_M_waiting())
	    return;
	}

      __platform_wait_t* __wait_addr;
      if (__args & __wait_flags::__proxy_wait)
	{
#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
	   __pool = (__pool == nullptr) ? &__waiter_pool_impl::_S_impl_for(__addr)
					: __pool;
#endif
	   __wait_addr = &__pool->_M_ver;
	   __atomic_fetch_add(__wait_addr, 1, __ATOMIC_RELAXED);
	   __all = true;
	 }

#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
      __platform_notify(__wait_addr, __all);
#else
      lock_guard<mutex> __l{ __pool->_M_mtx };
      __pool->_M_cv.notify_all();
#endif
    }
  } // namespace __detail

  template<typename _Tp,
	   typename _Pred, typename _ValFn>
    void
    __atomic_wait_address(const _Tp* __addr,
			  _Pred&& __pred, _ValFn&& __vfn,
			  bool __bare_wait = false) noexcept
    {
      const auto __wait_addr =
	  reinterpret_cast<const __detail::__platform_wait_t*>(__addr);
      __detail::__wait_args __args{ __addr, __bare_wait };
      _Tp __val = __vfn();
      while (!__pred(__val))
	{
	  __detail::__wait_impl(__wait_addr, __args);
	  __val = __vfn();
	}
      // C++26 will return __val
    }

  inline void
  __atomic_wait_address_v(const __detail::__platform_wait_t* __addr,
			  __detail::__platform_wait_t __old,
			  int __order)
  {
    __detail::__wait_args __args{ __addr, __old, __order };
    // C++26 will not ignore the return value here
    __detail::__wait_impl(__addr, __args);
  }

  template<typename _Tp, typename _ValFn>
    void
    __atomic_wait_address_v(const _Tp* __addr, _Tp __old,
			    _ValFn __vfn) noexcept
    {
      auto __pfn = [&](const _Tp& __val)
	  { return !__detail::__atomic_compare(__old, __val); };
      __atomic_wait_address(__addr, __pfn, forward<_ValFn>(__vfn));
    }

  template<typename _Tp>
    void
    __atomic_notify_address(const _Tp* __addr, bool __all,
			    bool __bare_wait = false) noexcept
    {
      const auto __wait_addr =
	  reinterpret_cast<const __detail::__platform_wait_t*>(__addr);
      __detail::__wait_args __args{ __addr, __bare_wait };
      __detail::__notify_impl(__wait_addr, __all, __args);
    }
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // __glibcxx_atomic_wait
#endif // _GLIBCXX_ATOMIC_WAIT_H
