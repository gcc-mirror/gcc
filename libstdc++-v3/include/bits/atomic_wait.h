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
    // TODO: this needs to be false for types with padding, e.g. __int20.
    // TODO: should this be true only for integral, enum, and pointer types?
    template<typename _Tp>
      concept __waitable
	= is_scalar_v<_Tp> && __builtin_popcountg(sizeof(_Tp)) == 1
	    && (sizeof(_Tp) <= sizeof(__UINT64_TYPE__));
  }

#if defined _GLIBCXX_HAVE_LINUX_FUTEX
  namespace __detail
  {
    // Use futex syscall on int objects.
    using __platform_wait_t = int;
    inline constexpr size_t __platform_wait_alignment = 4;
  }
  // Defined to true for a subset of __waitable types which are statically
  // known to definitely be able to use futex, not a proxy wait.
  template<typename _Tp>
    inline constexpr bool __platform_wait_uses_type
      = __detail::__waitable<_Tp>
	  && sizeof(_Tp) == sizeof(int) && alignof(_Tp) >= 4;
#else
// define _GLIBCX_HAVE_PLATFORM_WAIT and implement __platform_wait()
// and __platform_notify() if there is a more efficient primitive supported
// by the platform (e.g. __ulock_wait()/__ulock_wake()) which is better than
// a mutex/condvar based wait.
  namespace __detail
  {
# if ATOMIC_LONG_LOCK_FREE == 2
    using __platform_wait_t = unsigned long;
# else
    using __platform_wait_t = unsigned int;
# endif
    inline constexpr size_t __platform_wait_alignment
      = sizeof(__platform_wait_t) < __alignof__(__platform_wait_t)
	  ? __alignof__(__platform_wait_t) : sizeof(__platform_wait_t);
  } // namespace __detail

  // This must be false for the general case where we don't know of any
  // futex-like syscall.
  template<typename>
    inline constexpr bool __platform_wait_uses_type = false;
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
	return __builtin_memcmp(std::addressof(__a), std::addressof(__b),
				sizeof(_Tp)) == 0;
      }

    // Storage for up to 64 bits of value, should be considered opaque bits.
    using __wait_value_type = __UINT64_TYPE__;

    // lightweight std::optional<__wait_value_type>
    struct __wait_result_type
    {
      __wait_value_type _M_val;
      unsigned char _M_has_val : 1; // _M_val value was loaded before return.
      unsigned char _M_timeout : 1; // Waiting function ended with timeout.
      unsigned char _M_unused : 6;  // padding
    };

    enum class __wait_flags : __UINT_LEAST32_TYPE__
    {
       __abi_version = 0x00000000,
       // currently unused = 1,
       __track_contention = 2,
       __do_spin = 4,
       __spin_only = 8, // Ignored unless __do_spin is also set.
       // __abi_version_mask = 0xff000000,
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
      int _M_order = __ATOMIC_ACQUIRE; // Memory order for loads from _M_obj.
      __wait_value_type _M_old = 0;  // Previous value of *_M_obj.
      void* _M_wait_state = nullptr; // For proxy wait and tracking contention.
      const void* _M_obj = nullptr;  // The address of the object to wait on.
      unsigned char _M_obj_size = 0; // The size of that object.

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
	{
	  _M_obj = __addr; // Might be replaced by _M_setup_wait
	  if constexpr (__waitable<_Tp>)
	    // __wait_impl might be able to wait directly on __addr
	    // instead of using a proxy, depending on its size.
	    _M_obj_size = sizeof(_Tp);
	}

      __wait_args(const __platform_wait_t* __addr, __platform_wait_t __old,
		  int __order, bool __bare_wait = false) noexcept
      : __wait_args(__addr, __bare_wait)
      {
	_M_order = __order;
	_M_old = __old;
      }

      __wait_args(const __wait_args&) noexcept = default;
      __wait_args& operator=(const __wait_args&) noexcept = default;

      template<typename _Tp, typename _ValFn>
	_Tp
	_M_setup_wait(const _Tp* __addr, _ValFn __vfn)
	{
	  static_assert(is_same_v<_Tp, decay_t<decltype(__vfn())>>);

	  if constexpr (!__platform_wait_uses_type<_Tp>)
	    if (_M_setup_proxy_wait(__addr))
	      {
		// We will use a proxy wait for this object.
		// The library has set _M_wait_state, _M_obj, _M_obj_size,
		// and _M_old.
		// Call __vfn to load the current value from *__addr
		// (which must happen after the call to _M_setup_proxy_wait).
		return __vfn();
	      }

	  // We will use a futex-like operation to wait on this object,
	  // so just load the value, store it into _M_old, and return it.
	  return _M_store(__vfn());
	}

      // Called after a wait returns, to prepare to wait again.
      template<typename _Tp, typename _ValFn>
	_Tp
	_M_on_wake(const _Tp* __addr, _ValFn __vfn, __wait_result_type __res)
	{
	  if constexpr (!__platform_wait_uses_type<_Tp>) // maybe a proxy wait
	    if (_M_obj != __addr) // definitely a proxy wait
	      {
		if (__res._M_has_val)
		  // Previous wait loaded a recent value from the proxy.
		  _M_old = __res._M_val;
		else // Load a new value from the proxy and store in _M_old.
		  (void) _M_setup_proxy_wait(nullptr);
		// Read the current value of *__addr
		return __vfn();
	      }

	  if (__res._M_has_val) // The previous wait loaded a recent value.
	    {
	      _M_old = __res._M_val;

	      // Not a proxy wait, so the value in __res._M_val was loaded
	      // from *__addr and we don't need to call __vfn().
	      if constexpr (sizeof(_Tp) == sizeof(__UINT64_TYPE__))
		return __builtin_bit_cast(_Tp, (__UINT64_TYPE__)_M_old);
	      else if constexpr (sizeof(_Tp) == sizeof(__UINT32_TYPE__))
		return __builtin_bit_cast(_Tp, (__UINT32_TYPE__)_M_old);
	      else if constexpr (sizeof(_Tp) == sizeof(__UINT16_TYPE__))
		return __builtin_bit_cast(_Tp, (__UINT16_TYPE__)_M_old);
	      else if constexpr (sizeof(_Tp) == sizeof(__UINT8_TYPE__))
		return __builtin_bit_cast(_Tp, (__UINT8_TYPE__)_M_old);
	      else // Should be a proxy wait for this size!
		__glibcxx_assert(false);
	    }
	  else
	    return _M_store(__vfn());
	}

    private:
      // Store __val in _M_old.
      // pre: This must be a non-proxy wait.
      template<typename _Tp>
	[[__gnu__::__always_inline__]]
	_Tp
	_M_store(_Tp __val)
	{
	  // We have to consider various sizes, because a future libstdc++.so
	  // might enable non-proxy waits for additional sizes.
	  if constexpr (sizeof(_Tp) == sizeof(__UINT64_TYPE__))
	    _M_old = __builtin_bit_cast(__UINT64_TYPE__, __val);
	  else if constexpr (sizeof(_Tp) == sizeof(__UINT32_TYPE__))
	    _M_old = __builtin_bit_cast(__UINT32_TYPE__, __val);
	  else if constexpr (sizeof(_Tp) == sizeof(__UINT16_TYPE__))
	    _M_old = __builtin_bit_cast(__UINT16_TYPE__, __val);
	  else if constexpr (sizeof(_Tp) == sizeof(__UINT8_TYPE__))
	    _M_old = __builtin_bit_cast(__UINT8_TYPE__, __val);
	  else // Should be a proxy wait for this size!
	    __glibcxx_assert(false);
	  return __val;
	}

      // Prepare `*this` for a call to `__wait_impl` or `__wait_until_impl`.
      // See comments in src/c++20/atomic.cc for more details.
      bool
      _M_setup_proxy_wait(const void* __addr);

      template<typename _Tp>
	static constexpr __wait_flags
	_S_flags_for(const _Tp*, bool __bare_wait) noexcept
	{
	  using enum __wait_flags;
	  __wait_flags __res = __abi_version | __do_spin;
	  if (!__bare_wait)
	    __res |= __track_contention;
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
  // The effect of __vfn() must be an atomic load from __addr and nothing else.
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
	  __val = __args._M_on_wake(__addr, __vfn, __res);
	}
      // C++26 will return __val
    }

  // Wait on __addr while *__addr == __old is true.
  inline void
  __atomic_wait_address_v(const __detail::__platform_wait_t* __addr,
			  __detail::__platform_wait_t __old,
			  int __order, bool __bare_wait = false)
  {
    // This function must not be used if __wait_impl might use a proxy wait:
    __glibcxx_assert(__platform_wait_uses_type<__detail::__platform_wait_t>);

    __detail::__wait_args __args{ __addr, __old, __order, __bare_wait };
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
