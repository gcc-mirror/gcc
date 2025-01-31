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
#include <bits/gthr.h>
#include <ext/numeric_traits.h>

#include <bits/stl_pair.h>

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
	&& (alignof(_Tp) >= __detail::__platform_wait_alignment));
#else
      = false;
#endif

  namespace __detail
  {
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

    // return true if equal
    template<typename _Tp>
      inline bool
      __atomic_eq(const _Tp& __a, const _Tp& __b)
      {
	// TODO make this do the correct padding bit ignoring comparison
	return __builtin_memcmp(&__a, &__b, sizeof(_Tp)) == 0;
      }

    // lightweight std::optional<__platform_wait_t>
    struct __wait_result_type
    {
      __platform_wait_t _M_val;
      unsigned char _M_has_val : 1; // _M_val value was loaded before return.
      unsigned char _M_timeout : 1; // Waiting function ended with timeout.
      unsigned char _M_unused : 6;  // padding
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
      void* _M_wait_state = nullptr;

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

      template<typename _ValFn,
	       typename _Tp = decay_t<decltype(std::declval<_ValFn&>()())>>
	_Tp
	_M_setup_wait(const void* __addr, _ValFn __vfn,
		      __wait_result_type __res = {})
	{
	  if constexpr (__platform_wait_uses_type<_Tp>)
	    {
	      // If the wait is not proxied, the value we check when waiting
	      // is the value of the atomic variable itself.

	      if (__res._M_has_val) // The previous wait loaded a recent value.
		{
		  _M_old = __res._M_val;
		  return __builtin_bit_cast(_Tp, __res._M_val);
		}
	      else // Load the value from __vfn
		{
		  _Tp __val = __vfn();
		  _M_old = __builtin_bit_cast(__platform_wait_t, __val);
		  return __val;
		}
	    }
	  else // It's a proxy wait and the proxy's _M_ver is used.
	    {
	      if (__res._M_has_val) // The previous wait loaded a recent value.
		_M_old = __res._M_val;
	      else // Load _M_ver from the proxy (must happen before __vfn()).
		_M_load_proxy_wait_val(__addr);
	      return __vfn();
	    }
	}

    private:
      // Populates _M_wait_state and _M_old from the proxy for __addr.
      void
      _M_load_proxy_wait_val(const void* __addr);

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
    };

    __wait_result_type
    __wait_impl(const void* __addr, __wait_args_base&);

    void
    __notify_impl(const void* __addr, bool __all, const __wait_args_base&);
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
      _Tp __val = __args._M_setup_wait(__addr, __vfn);
      while (!__pred(__val))
	{
	  auto __res = __detail::__wait_impl(__addr, __args);
	  __val = __args._M_setup_wait(__addr, __vfn, __res);
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
      std::__atomic_wait_address(__addr, __pfn, forward<_ValFn>(__vfn));
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
