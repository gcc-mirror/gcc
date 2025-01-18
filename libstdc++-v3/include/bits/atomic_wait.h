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

    // If the futex *__addr is equal to __val, wait on the futex until woken.
    inline void
    __platform_wait(const int* __addr, int __val) noexcept
    {
      auto __e = syscall (SYS_futex, __addr,
			  static_cast<int>(__futex_wait_flags::__wait_private),
			  __val, nullptr);
      if (!__e || errno == EAGAIN)
	return;
      if (errno != EINTR)
	__throw_system_error(errno);
    }

    // Wake threads waiting on the futex *__addr.
    inline void
    __platform_notify(const int* __addr, bool __all) noexcept
    {
      syscall (SYS_futex, __addr,
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
      inline bool
      __atomic_eq(const _Tp& __a, const _Tp& __b)
      {
	// TODO make this do the correct padding bit ignoring comparison
	return __builtin_memcmp(&__a, &__b, sizeof(_Tp)) == 0;
      }

    struct __wait_args_base;

    // The state used by atomic waiting and notifying functions.
    struct __waitable_state
    {
      // Don't use std::hardware_destructive_interference_size here because we
      // don't want the layout of library types to depend on compiler options.
      static constexpr auto _S_align = 64;

      // Count of threads blocked waiting on this state.
      alignas(_S_align) __platform_wait_t _M_waiters = 0;

#ifndef _GLIBCXX_HAVE_PLATFORM_WAIT
      mutex _M_mtx;
#endif

      // If we can't do a platform wait on the atomic variable itself,
      // we use this member as a proxy for the atomic variable and we
      // use this for waiting and notifying functions instead.
      alignas(_S_align) __platform_wait_t _M_ver = 0;

#ifndef _GLIBCXX_HAVE_PLATFORM_WAIT
      __condvar _M_cv;
#endif

      __waitable_state() = default;

      void
      _M_enter_wait() noexcept
      { __atomic_fetch_add(&_M_waiters, 1, __ATOMIC_SEQ_CST); }

      void
      _M_leave_wait() noexcept
      { __atomic_fetch_sub(&_M_waiters, 1, __ATOMIC_RELEASE); }

      bool
      _M_waiting() const noexcept
      {
	__platform_wait_t __res;
	__atomic_load(&_M_waiters, &__res, __ATOMIC_SEQ_CST);
	return __res != 0;
      }

      static __waitable_state&
      _S_state_for(const void* __addr) noexcept
      {
	constexpr __UINTPTR_TYPE__ __ct = 16;
	static __waitable_state __w[__ct];
	auto __key = ((__UINTPTR_TYPE__)__addr >> 2) % __ct;
	return __w[__key];
      }

      // Return an RAII type that calls _M_enter_wait() on construction
      // and _M_leave_wait() on destruction.
      static auto
      _S_track(__waitable_state*& __state, const __wait_args_base& __args,
	       const void* __addr) noexcept;
    };

    enum class __wait_flags : __UINT_LEAST32_TYPE__
    {
       __abi_version = 0,
       __proxy_wait = 1,
       __track_contention = 2,
       __do_spin = 4,
       __spin_only = 8, // Ignored unless __do_spin is also set.
       // __abi_version_mask = 0xffff0000,
    };

    [[__gnu__::__always_inline__]]
    constexpr __wait_flags
    operator|(__wait_flags __l, __wait_flags __r) noexcept
    {
      using _Ut = underlying_type_t<__wait_flags>;
      return static_cast<__wait_flags>(static_cast<_Ut>(__l)
					 | static_cast<_Ut>(__r));
    }

    [[__gnu__::__always_inline__]]
    constexpr __wait_flags&
    operator|=(__wait_flags& __l, __wait_flags __r) noexcept
    { return __l = __l | __r; }

    // Simple aggregate containing arguments used by implementation details.
    struct __wait_args_base
    {
      __wait_flags _M_flags;
      int _M_order = __ATOMIC_ACQUIRE;
      __platform_wait_t _M_old = 0;

      // Test whether _M_flags & __flags is non-zero.
      bool
      operator&(__wait_flags __flags) const noexcept
      {
	 using _Ut = underlying_type_t<__wait_flags>;
	 return static_cast<_Ut>(_M_flags) & static_cast<_Ut>(__flags);
      }
    };

    // Utility for populating a __wait_args_base structure.
    struct __wait_args : __wait_args_base
    {
      template<typename _Tp> requires (!is_same_v<_Tp, __wait_args>)
	explicit
	__wait_args(const _Tp* __addr, bool __bare_wait = false) noexcept
	: __wait_args_base{ _S_flags_for(__addr, __bare_wait) }
	{ }

      __wait_args(const __platform_wait_t* __addr, __platform_wait_t __old,
		  int __order, bool __bare_wait = false) noexcept
      : __wait_args_base{ _S_flags_for(__addr, __bare_wait), __order, __old }
      { }

      __wait_args(const __wait_args&) noexcept = default;
      __wait_args& operator=(const __wait_args&) noexcept = default;

    private:
      template<typename _Tp>
	static constexpr __wait_flags
	_S_flags_for(const _Tp*, bool __bare_wait) noexcept
	{
	  using enum __wait_flags;
	  __wait_flags __res = __abi_version | __do_spin;
	  if (!__bare_wait)
	    __res |= __track_contention;
	  if constexpr (!__platform_wait_uses_type<_Tp>)
	    __res |= __proxy_wait;
	  return __res;
	}

      // XXX what is this for? It's never used.
      template<typename _Tp>
	static int
	_S_memory_order_for(const _Tp*, int __order) noexcept
	{
	  if constexpr (__platform_wait_uses_type<_Tp>)
	    return __order;
	  return __ATOMIC_ACQUIRE;
	}
    };

    inline auto
    __waitable_state::_S_track(__waitable_state*& __state,
			       const __wait_args_base& __args,
			       const void* __addr) noexcept
    {
      struct _Tracker
      {
	_Tracker() noexcept : _M_st(nullptr) { }

	[[__gnu__::__nonnull__]]
	explicit
	_Tracker(__waitable_state* __st) noexcept
	: _M_st(__st)
	{ __st->_M_enter_wait(); }

	_Tracker(const _Tracker&) = delete;
	_Tracker& operator=(const _Tracker&) = delete;

	~_Tracker() { if (_M_st) _M_st->_M_leave_wait(); }

	__waitable_state* _M_st;
      };

      if (__args & __wait_flags::__track_contention)
	{
	  // Caller does not externally track contention,
	  // so we want to increment+decrement __state->_M_waiters

	  // First make sure we have a waitable state for the address.
	  if (!__state)
	    __state = &__waitable_state::_S_state_for(__addr);

	  // This object will increment the number of waiters and
	  // decrement it again on destruction.
	  return _Tracker{__state};
	}
      return _Tracker{}; // For bare waits caller tracks waiters.
    }

    using __wait_result_type = pair<bool, __platform_wait_t>;

    inline __wait_result_type
    __spin_impl(const __platform_wait_t* __addr, const __wait_args_base& __args)
    {
      __platform_wait_t __val;
      for (auto __i = 0; __i < __atomic_spin_count; ++__i)
	{
	  __atomic_load(__addr, &__val, __args._M_order);
	  if (__val != __args._M_old)
	    return { true, __val };
	  if (__i < __atomic_spin_count_relax)
	    __detail::__thread_relax();
	  else
	    __detail::__thread_yield();
	}
      return { false, __val };
    }

    inline __wait_result_type
    __wait_impl(const void* __addr, const __wait_args_base& __a)
    {
      __wait_args_base __args = __a;
      __waitable_state* __state = nullptr;

      const __platform_wait_t* __wait_addr;
      if (__args & __wait_flags::__proxy_wait)
	{
	  __state = &__waitable_state::_S_state_for(__addr);
	  __wait_addr = &__state->_M_ver;
	  __atomic_load(__wait_addr, &__args._M_old, __args._M_order);
	}
      else
	__wait_addr = static_cast<const __platform_wait_t*>(__addr);

      if (__args & __wait_flags::__do_spin)
	{
	  auto __res = __detail::__spin_impl(__wait_addr, __args);
	  if (__res.first)
	    return __res;
	  if (__args & __wait_flags::__spin_only)
	    return __res;
	}

      auto __tracker = __waitable_state::_S_track(__state, __args, __addr);

#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
      __platform_wait(__wait_addr, __args._M_old);
      return { false, __args._M_old };
#else
      __platform_wait_t __val;
      __atomic_load(__wait_addr, &__val, __args._M_order);
      if (__val == __args._M_old)
	{
	  if (!__state)
	    __state = &__waitable_state::_S_state_for(__addr);
	  lock_guard<mutex> __l{ __state->_M_mtx };
	  __atomic_load(__wait_addr, &__val, __args._M_order);
	  if (__val == __args._M_old)
	    __state->_M_cv.wait(__state->_M_mtx);
	}
      return { false, __val };
#endif
    }

    inline void
    __notify_impl(const void* __addr, [[maybe_unused]] bool __all,
		  const __wait_args_base& __args)
    {
      __waitable_state* __state = nullptr;

      const __platform_wait_t* __wait_addr;
      if (__args & __wait_flags::__proxy_wait)
	{
	  __state = &__waitable_state::_S_state_for(__addr);
	  // Waiting for *__addr is actually done on the proxy's _M_ver.
	  __wait_addr = &__state->_M_ver;
	  __atomic_fetch_add(&__state->_M_ver, 1, __ATOMIC_RELAXED);
	  // Because the proxy might be shared by several waiters waiting
	  // on different atomic variables, we need to wake them all so
	  // they can re-evaluate their conditions to see if they should
	  // stop waiting or should wait again.
	  __all = true;
	}
      else // Use the atomic variable's own address.
	__wait_addr = static_cast<const __platform_wait_t*>(__addr);

      if (__args & __wait_flags::__track_contention)
	{
	  if (!__state)
	    __state = &__waitable_state::_S_state_for(__addr);
	  if (!__state->_M_waiting())
	    return;
	}

#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
      __platform_notify(__wait_addr, __all);
#else
      if (!__state)
	__state = &__waitable_state::_S_state_for(__addr);
      lock_guard<mutex> __l{ __state->_M_mtx };
      __state->_M_cv.notify_all();
#endif
    }
  } // namespace __detail

  // Wait on __addr while __pred(__vfn()) is false.
  // If __bare_wait is false, increment a counter while waiting.
  // For callers that keep their own count of waiters, use __bare_wait=true.
  template<typename _Tp, typename _Pred, typename _ValFn>
    void
    __atomic_wait_address(const _Tp* __addr, _Pred&& __pred, _ValFn&& __vfn,
			  bool __bare_wait = false) noexcept
    {
      __detail::__wait_args __args{ __addr, __bare_wait };
      _Tp __val = __vfn();
      while (!__pred(__val))
	{
	  // If the wait is not proxied, set the value that we're waiting
	  // to change.
	  if constexpr (__platform_wait_uses_type<_Tp>)
	    __args._M_old = __builtin_bit_cast(__detail::__platform_wait_t,
					       __val);
	  // Otherwise, it's a proxy wait and the proxy's _M_ver is used.

	  __detail::__wait_impl(__addr, __args);
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

  // Wait on __addr while __vfn() == __old is true.
  template<typename _Tp, typename _ValFn>
    void
    __atomic_wait_address_v(const _Tp* __addr, _Tp __old,
			    _ValFn __vfn) noexcept
    {
      auto __pfn = [&](const _Tp& __val)
	  { return !__detail::__atomic_eq(__old, __val); };
      __atomic_wait_address(__addr, __pfn, forward<_ValFn>(__vfn));
    }

  template<typename _Tp>
    void
    __atomic_notify_address(const _Tp* __addr, bool __all,
			    bool __bare_wait = false) noexcept
    {
      __detail::__wait_args __args{ __addr, __bare_wait };
      __detail::__notify_impl(__addr, __all, __args);
    }
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // __glibcxx_atomic_wait
#endif // _GLIBCXX_ATOMIC_WAIT_H
