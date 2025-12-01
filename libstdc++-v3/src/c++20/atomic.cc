// Definitions for <atomic> wait/notify -*- C++ -*-

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

#include <bits/version.h>

#if __glibcxx_atomic_wait
#include <atomic>
#include <bits/atomic_timed_wait.h>
#include <cstdint> // uint32_t, uint64_t
#include <climits> // INT_MAX
#include <cerrno>  // errno, ETIMEDOUT, etc.
#include <bits/std_mutex.h>  // std::mutex, std::__condvar
#include <bits/functexcept.h> // __throw_system_error
#include <bits/functional_hash.h>

#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
# include <sys/syscall.h> // SYS_futex
# include <unistd.h>
# include <sys/time.h> // timespec
# define _GLIBCXX_HAVE_PLATFORM_WAIT 1
#endif

#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace __detail
{
namespace
{
#ifndef _GLIBCXX_HAVE_PLATFORM_WAIT

  // If _GLIBCXX_HAVE_PLATFORM_WAIT is defined then the following three
  // functions should be defined in terms of platform-specific wait/wake
  // primitives. The `obj_size` parameter is the size in bytes of the object
  // at `*addr` (used if the platform supports waiting on more than one size,
  // in which case `addr` would be cast to a different type).

  // Deleted definitions are here to give better errors if these functions
  // are used when _GLIBCXX_HAVE_PLATFORM_WAIT is not defined.

  // Wait until *addr != curr_val.
  // Once a thread is waiting, it will not unblock and notice the value
  // has changed unless explicitly notified using `__platform_notify`.
  void
  __platform_wait(const __platform_wait_t* addr,
		  __platform_wait_t curr_val,
		  int obj_size) = delete;

  // Wake one thread that is waiting for `*addr` to change,
  // or all waiting threads if `wake_all` is true.
  void
  __platform_notify(const __platform_wait_t* addr,
		    bool wake_all,
		    int obj_size) = delete;

  // As `__platform_wait` but with timeout.
  // Returns true if the wait ended before the timeout (which could be because
  // the value changed and __platform_notify was called, but could be because
  // the wait was interrupted by a signal, or just a spurious wake).
  // Returns false if the timeout was reached.
  bool
  __platform_wait_until(const __platform_wait_t* addr,
			__platform_wait_t curr_val,
			__wait_clock_t::time_point timeout,
			int obj_size) = delete;

#elif defined _GLIBCXX_HAVE_LINUX_FUTEX

  const int futex_private_flag = 128;

  void
  __platform_wait(const int* addr, int val, int /* obj_size */) noexcept
  {
    const int futex_op_wait = 0;
    const int futex_op_wait_private = futex_op_wait | futex_private_flag;

    if (syscall(SYS_futex, addr, futex_op_wait_private, val, nullptr))
      if (errno != EAGAIN && errno != EINTR)
	__throw_system_error(errno);
  }

  void
  __platform_notify(const int* addr, bool all, int /* obj_size */) noexcept
  {
    const int futex_op_wake = 1;
    const int futex_op_wake_private = futex_op_wake | futex_private_flag;

    syscall(SYS_futex, addr, futex_op_wake_private, all ? INT_MAX : 1);
  }

  // returns true if wait ended before timeout
  bool
  __platform_wait_until(const int* addr, int val,
			const __wait_clock_t::time_point& abs_time,
			int /* obj_size */) noexcept
  {
    // FUTEX_WAIT expects a relative timeout, so must use FUTEX_WAIT_BITSET
    // for an absolute timeout.
    const int futex_op_wait_bitset = 9;
    const int futex_op_wait_bitset_private
      = futex_op_wait_bitset | futex_private_flag;
    const int futex_bitset_match_any = 0xffffffff;

    struct timespec timeout = chrono::__to_timeout_timespec(abs_time);

    if (syscall(SYS_futex, addr, futex_op_wait_bitset_private, val,
		&timeout, nullptr, futex_bitset_match_any))
      {
	if (errno == ETIMEDOUT)
	  return false;
	if (errno != EAGAIN && errno != EINTR)
	  __throw_system_error(errno);
      }
    return true;
  }
#endif // HAVE_PLATFORM_WAIT

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

    // This type meets the Cpp17BasicLockable requirements.
    void lock() { _M_mtx.lock(); }
    void unlock() { _M_mtx.unlock(); }
#else
    void lock() { }
    void unlock() { }
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
  };

  // Scope-based contention tracking.
  struct scoped_wait
  {
    // pre: if track_contention is in flags, then args._M_wait_state != nullptr
    explicit
    scoped_wait(const __wait_args_base& args) : _M_state(nullptr)
    {
      if (args & __wait_flags::__track_contention)
	{
	  _M_state = static_cast<__waitable_state*>(args._M_wait_state);
	  _M_state->_M_enter_wait();
	}
    }

    ~scoped_wait()
    {
      if (_M_state)
	_M_state->_M_leave_wait();
    }

    scoped_wait(scoped_wait&&) = delete;

    __waitable_state* _M_state;
  };

  // Scoped lock type
  struct waiter_lock
  {
    // pre: args._M_state != nullptr
    explicit
    waiter_lock(const __wait_args_base& args)
    : _M_state(*static_cast<__waitable_state*>(args._M_wait_state)),
      _M_track_contention(args & __wait_flags::__track_contention)
    {
      _M_state.lock();
      if (_M_track_contention)
	_M_state._M_enter_wait();
    }

    waiter_lock(waiter_lock&&) = delete;

    ~waiter_lock()
    {
      if (_M_track_contention)
	_M_state._M_leave_wait();
      _M_state.unlock();
    }

    __waitable_state& _M_state;
    bool _M_track_contention;
  };

  constexpr auto __atomic_spin_count_relax = 12;
  constexpr auto __atomic_spin_count = 16;

  // This function always returns _M_has_val == true and _M_val == *__addr.
  // _M_timeout == (*__addr == __args._M_old).
  __wait_result_type
  __spin_impl(const __platform_wait_t* __addr, const __wait_args_base& __args)
  {
    __wait_value_type wval;
    for (auto __i = 0; __i < __atomic_spin_count; ++__i)
      {
	wval = __atomic_load_n(__addr, __args._M_order);
	if (wval != __args._M_old)
	  return { ._M_val = wval, ._M_has_val = true, ._M_timeout = false };
	if (__i < __atomic_spin_count_relax)
	  __thread_relax();
	else
	  __thread_yield();
      }
    return { ._M_val = wval, ._M_has_val = true, ._M_timeout = true };
  }

  inline __waitable_state*
  set_wait_state(const void* addr, __wait_args_base& args)
  {
    if (args._M_wait_state == nullptr)
      args._M_wait_state = &__waitable_state::_S_state_for(addr);
    return static_cast<__waitable_state*>(args._M_wait_state);
  }

  [[gnu::always_inline]]
  inline bool
  use_proxy_wait([[maybe_unused]] const __wait_args_base& args,
		 [[maybe_unused]] const void* /* addr */)
  {
#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
    if constexpr (__platform_wait_uses_type<uint32_t>)
      if (args._M_obj_size == sizeof(uint32_t))
	return false;

    if constexpr (__platform_wait_uses_type<uint64_t>)
      if (args._M_obj_size == sizeof(uint64_t))
	return false;

    // __wait_args::_M_old can only hold 64 bits, so larger types
    // must always use a proxy wait.
    if (args._M_obj_size > sizeof(uint64_t))
      return true;

    // __wait_args::_M_setup_wait only knows how to store 1/2/4/8 byte types,
    // so anything else must always use a proxy wait.
    if (__builtin_popcountg(args._M_obj_size) != 1)
      return true;
#endif

    // Currently use proxy wait for everything else:
    return true;
  }

} // namespace

// Return false (and don't change any data members) if we can do a non-proxy
// wait for the object of size `_M_obj_size` at address `addr`.
// Otherwise, the object at addr needs to use a proxy wait. Set _M_wait_state,
// set _M_obj and _M_obj_size to refer to the _M_wait_state->_M_ver proxy,
// load the current value from _M_obj and store it in _M_old, then return true.
bool
__wait_args::_M_setup_proxy_wait(const void* addr)
{
  if (!use_proxy_wait(*this, addr)) // We can wait on this address directly.
    {
      // Ensure the caller set _M_obj correctly, as that's what we'll wait on:
      __glibcxx_assert(_M_obj == addr);
      return false;
    }

  // This will be a proxy wait, so get a waitable state.
  auto state = set_wait_state(addr, *this);

  // The address we will wait on is the version count of the waitable state:
  _M_obj = &state->_M_ver;
  // __wait_impl and __wait_until_impl need to know this size:
  _M_obj_size = sizeof(state->_M_ver);

  // Read the value of the _M_ver counter.
  _M_old = __atomic_load_n(&state->_M_ver, __ATOMIC_ACQUIRE);

  return true;
}

__wait_result_type
__wait_impl([[maybe_unused]] const void* __addr, __wait_args_base& __args)
{
  auto* __wait_addr = static_cast<const __platform_wait_t*>(__args._M_obj);

  if (__args & __wait_flags::__do_spin)
    {
      auto __res = __detail::__spin_impl(__wait_addr, __args);
      if (!__res._M_timeout)
	return __res;
      if (__args & __wait_flags::__spin_only)
	return __res;
    }

#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
  if (__args & __wait_flags::__track_contention)
    set_wait_state(__addr, __args); // scoped_wait needs a __waitable_state
  scoped_wait s(__args);
  __platform_wait(__wait_addr, __args._M_old, __args._M_obj_size);
  // We haven't loaded a new value so return _M_has_val=false
  return { ._M_val = __args._M_old, ._M_has_val = false, ._M_timeout = false };
#else
  waiter_lock l(__args);
  __platform_wait_t __val;
  __atomic_load(__wait_addr, &__val, __args._M_order);
  if (__val == __args._M_old)
    {
      auto __state = static_cast<__waitable_state*>(__args._M_wait_state);
      __state->_M_cv.wait(__state->_M_mtx);
      return { ._M_val = __val, ._M_has_val = false, ._M_timeout = false };
    }
  return { ._M_val = __val, ._M_has_val = true, ._M_timeout = false };
#endif
}

void
__notify_impl([[maybe_unused]] const void* __addr, [[maybe_unused]] bool __all,
	      const __wait_args_base& __args)
{
  const bool __track_contention = __args & __wait_flags::__track_contention;
  const bool proxy_wait = use_proxy_wait(__args, __addr);

  [[maybe_unused]] auto* __wait_addr
    = static_cast<const __platform_wait_t*>(__addr);

#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
  // Check whether it would be a non-proxy wait for this object.
  // This condition must match the one in _M_setup_wait_impl to ensure that
  // the address used for the notify matches the one used for the wait.
  if (!proxy_wait)
    {
      if (__track_contention)
	if (!__waitable_state::_S_state_for(__addr)._M_waiting())
	  return;

      __platform_notify(__wait_addr, __all, __args._M_obj_size);
      return;
    }
#endif

  // Either a proxy wait or we don't have platform wait/wake primitives.

  auto __state = &__waitable_state::_S_state_for(__addr);

  // Lock mutex so that proxied waiters cannot race with incrementing _M_ver
  // and see the old value, then sleep after the increment and notify_all().
  lock_guard __l{ *__state };

  if (proxy_wait)
    {
      // Increment _M_ver so that waiting threads see something changed.
      // This has to be atomic because the load in _M_load_proxy_wait_val
      // is done without the mutex locked.
      __atomic_fetch_add(&__state->_M_ver, 1, __ATOMIC_RELEASE);

      // Because the proxy might be shared by several waiters waiting
      // on different atomic variables, we need to wake them all so
      // they can re-evaluate their conditions to see if they should
      // stop waiting or should wait again.
      __all = true;

      __wait_addr = &__state->_M_ver;
    }

  if (__track_contention)
    {
      if (!__state->_M_waiting())
	return;
    }

#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
  __platform_notify(__wait_addr, __all, sizeof(__state->_M_ver));
#else
  __state->_M_cv.notify_all();
#endif
}

// Timed atomic waiting functions

namespace
{
#ifndef _GLIBCXX_HAVE_PLATFORM_WAIT
bool
__cond_wait_until(__condvar& __cv, mutex& __mx,
		  const __wait_clock_t::time_point& __atime)
{
  __gthread_time_t __ts = chrono::__to_timeout_gthread_time_t(__atime);

#ifdef _GLIBCXX_USE_PTHREAD_COND_CLOCKWAIT
  if constexpr (is_same_v<chrono::steady_clock, __wait_clock_t>)
    __cv.wait_until(__mx, CLOCK_MONOTONIC, __ts);
  else
#endif
    __cv.wait_until(__mx, __ts);
  return __wait_clock_t::now() < __atime;
}
#endif // ! HAVE_PLATFORM_WAIT
} // namespace

__wait_result_type
__wait_until_impl([[maybe_unused]] const void* __addr, __wait_args_base& __args,
		  const chrono::nanoseconds& __time)
{
  const __wait_clock_t::time_point __atime(__time);
  auto* __wait_addr = static_cast<const __platform_wait_t*>(__args._M_obj);

  if (__args & __wait_flags::__do_spin)
    {
      auto __res = __detail::__spin_impl(__wait_addr, __args);
      if (!__res._M_timeout)
	return __res;
      if (__args & __wait_flags::__spin_only)
	return __res;
      if (__wait_clock_t::now() >= __atime)
	return __res;
    }

#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
  if (__args & __wait_flags::__track_contention)
    set_wait_state(__addr, __args); // scoped_wait needs a __waitable_state
  scoped_wait s(__args);
  bool timeout = !__platform_wait_until(__wait_addr, __args._M_old, __atime,
					__args._M_obj_size);
  return { ._M_val = __args._M_old, ._M_has_val = false, ._M_timeout = timeout };
#else
  waiter_lock l(__args);
  __platform_wait_t __val;
  __atomic_load(__wait_addr, &__val, __args._M_order);
  if (__val == __args._M_old)
    {
      auto __state = static_cast<__waitable_state*>(__args._M_wait_state);
      bool timeout = !__cond_wait_until(__state->_M_cv, __state->_M_mtx, __atime);
      return { ._M_val = __val, ._M_has_val = false, ._M_timeout = timeout };
    }
  return { ._M_val = __val, ._M_has_val = true, ._M_timeout = false };
#endif
}

} // namespace __detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif
