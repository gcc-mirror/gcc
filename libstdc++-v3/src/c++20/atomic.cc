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
#include <bits/functional_hash.h>
#include <cstdint>
#include <bits/std_mutex.h>  // std::mutex, std::__condvar

#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
# include <cerrno>
# include <climits>
# include <unistd.h>
# include <syscall.h>
# include <bits/functexcept.h>
# include <sys/time.h>
#endif

#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
# ifndef _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT
// __waitable_state assumes that we consistently use the same implementation
// (i.e. futex vs mutex+condvar) for timed and untimed waiting.
#  error "This configuration is not currently supported"
# endif
#endif

#pragma GCC diagnostic ignored "-Wmissing-field-initializers"

namespace std
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace __detail
{
namespace
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

  void
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

  void
  __platform_notify(const int* __addr, bool __all) noexcept
  {
    syscall (SYS_futex, __addr,
	     static_cast<int>(__futex_wait_flags::__wake_private),
	     __all ? INT_MAX : 1);
  }
#endif

  // The state used by atomic waiting and notifying functions.
  struct __waitable_state
  {
    // Don't use std::hardware_destructive_interference_size here because we
    // don't want the layout of library types to depend on compiler options.
    static constexpr auto _S_align = 64;

    // Count of threads blocked waiting on this state.
    alignas(_S_align) __platform_wait_t _M_waiters = 0;

#ifndef _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT
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

#ifndef _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT
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
    __platform_wait_t __val{};
    for (auto __i = 0; __i < __atomic_spin_count; ++__i)
      {
	__atomic_load(__addr, &__val, __args._M_order);
	if (__val != __args._M_old)
	  return { ._M_val = __val, ._M_has_val = true, ._M_timeout = false };
	if (__i < __atomic_spin_count_relax)
	  __thread_relax();
	else
	  __thread_yield();
      }
    return { ._M_val = __val, ._M_has_val = true, ._M_timeout = true };
  }

  inline __waitable_state*
  set_wait_state(const void* addr, __wait_args_base& args)
  {
    if (args._M_wait_state == nullptr)
      args._M_wait_state = &__waitable_state::_S_state_for(addr);
    return static_cast<__waitable_state*>(args._M_wait_state);
  }

} // namespace

// Called for a proxy wait
void
__wait_args::_M_load_proxy_wait_val(const void* addr)
{
  // __glibcxx_assert( *this & __wait_flags::__proxy_wait );

  // We always need a waitable state for proxy waits.
  auto state = set_wait_state(addr, *this);

  // Read the value of the _M_ver counter.
  __atomic_load(&state->_M_ver, &_M_old, __ATOMIC_ACQUIRE);
}

__wait_result_type
__wait_impl(const void* __addr, __wait_args_base& __args)
{
  auto __state = static_cast<__waitable_state*>(__args._M_wait_state);

  const __platform_wait_t* __wait_addr;

  if (__args & __wait_flags::__proxy_wait)
    __wait_addr = &__state->_M_ver;
  else
    __wait_addr = static_cast<const __platform_wait_t*>(__addr);

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
  __platform_wait(__wait_addr, __args._M_old);
  // We haven't loaded a new value so return false as first member:
  return { ._M_val = __args._M_old, ._M_has_val = false, ._M_timeout = false };
#else
  waiter_lock l(__args);
  __platform_wait_t __val;
  __atomic_load(__wait_addr, &__val, __args._M_order);
  if (__val == __args._M_old)
    {
      __state->_M_cv.wait(__state->_M_mtx);
      return { ._M_val = __val, ._M_has_val = false, ._M_timeout = false };
    }
  return { ._M_val = __val, ._M_has_val = true, ._M_timeout = false };
#endif
}

void
__notify_impl(const void* __addr, [[maybe_unused]] bool __all,
	      const __wait_args_base& __args)
{
  auto __state = static_cast<__waitable_state*>(__args._M_wait_state);
  if (!__state)
    __state = &__waitable_state::_S_state_for(__addr);

  [[maybe_unused]] const __platform_wait_t* __wait_addr;

  // Lock mutex so that proxied waiters cannot race with incrementing _M_ver
  // and see the old value, then sleep after the increment and notify_all().
  lock_guard __l{ *__state };

  if (__args & __wait_flags::__proxy_wait)
    {
      // Waiting for *__addr is actually done on the proxy's _M_ver.
      __wait_addr = &__state->_M_ver;

      // Increment _M_ver so that waiting threads see something changed.
      // This has to be atomic because the load in _M_load_proxy_wait_val
      // is done without the mutex locked.
      __atomic_fetch_add(&__state->_M_ver, 1, __ATOMIC_RELEASE);

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
      if (!__state->_M_waiting())
	return;
    }

#ifdef _GLIBCXX_HAVE_PLATFORM_WAIT
  __platform_notify(__wait_addr, __all);
#else
  __state->_M_cv.notify_all();
#endif
}

// Timed atomic waiting functions

namespace
{
#ifdef _GLIBCXX_HAVE_LINUX_FUTEX
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

  if (syscall (SYS_futex, __addr,
	       static_cast<int>(__futex_wait_flags::__wait_bitset_private),
	       __old, &__rt, nullptr,
	       static_cast<int>(__futex_wait_flags::__bitset_match_any)))
    {
      if (errno == ETIMEDOUT)
	return false;
      if (errno != EINTR && errno != EAGAIN)
	__throw_system_error(errno);
    }
  return true;
}
#endif // HAVE_LINUX_FUTEX

#ifndef _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT
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
#endif // ! HAVE_PLATFORM_TIMED_WAIT

// Like __spin_impl, always returns _M_has_val == true.
__wait_result_type
__spin_until_impl(const __platform_wait_t* __addr,
		  const __wait_args_base& __args,
		  const __wait_clock_t::time_point& __deadline)
{
  auto __t0 = __wait_clock_t::now();
  using namespace literals::chrono_literals;

  __platform_wait_t __val{};
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
	  auto __res = __detail::__spin_impl(__addr, __args);
	  if (!__res._M_timeout)
	    return __res;
	}

      __atomic_load(__addr, &__val, __args._M_order);
      if (__val != __args._M_old)
	return { ._M_val = __val, ._M_has_val = true, ._M_timeout = false };
    }
  return { ._M_val = __val, ._M_has_val = true, ._M_timeout = true };
}
} // namespace

__wait_result_type
__wait_until_impl(const void* __addr, __wait_args_base& __args,
		  const __wait_clock_t::duration& __time)
{
  const __wait_clock_t::time_point __atime(__time);
  auto __state = static_cast<__waitable_state*>(__args._M_wait_state);
  const __platform_wait_t* __wait_addr;
  if (__args & __wait_flags::__proxy_wait)
    __wait_addr = &__state->_M_ver;
  else
    __wait_addr = static_cast<const __platform_wait_t*>(__addr);

  if (__args & __wait_flags::__do_spin)
    {
      auto __res = __detail::__spin_until_impl(__wait_addr, __args, __atime);
      if (!__res._M_timeout)
	return __res;
      if (__args & __wait_flags::__spin_only)
	return __res;
    }

#ifdef _GLIBCXX_HAVE_PLATFORM_TIMED_WAIT
  if (__args & __wait_flags::__track_contention)
    set_wait_state(__addr, __args);
  scoped_wait s(__args);
  if (__platform_wait_until(__wait_addr, __args._M_old, __atime))
    return { ._M_val = __args._M_old, ._M_has_val = false, ._M_timeout = false };
  else
    return { ._M_val = __args._M_old, ._M_has_val = false, ._M_timeout = true };
#else
  waiter_lock l(__args);
  __platform_wait_t __val;
  __atomic_load(__wait_addr, &__val, __args._M_order);
  if (__val == __args._M_old)
    {
      if (__cond_wait_until(__state->_M_cv, __state->_M_mtx, __atime))
	return { ._M_val = __val, ._M_has_val = false, ._M_timeout = false };
      else
	return { ._M_val = __val, ._M_has_val = false, ._M_timeout = true };
    }
  return { ._M_val = __val, ._M_has_val = true, ._M_timeout = false };
#endif
}

} // namespace __detail
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif
