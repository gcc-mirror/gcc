// futex -*- C++ -*-

// Copyright (C) 2015-2020 Free Software Foundation, Inc.
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

#include <bits/atomic_futex.h>
#ifdef _GLIBCXX_HAS_GTHREADS
#if defined(_GLIBCXX_HAVE_LINUX_FUTEX) && ATOMIC_INT_LOCK_FREE > 1
#include <chrono>
#include <climits>
#include <syscall.h>
#include <unistd.h>
#include <sys/time.h>
#include <errno.h>
#include <ext/numeric_traits.h>
#include <debug/debug.h>

// Constants for the wait/wake futex syscall operations
const unsigned futex_wait_op = 0;
const unsigned futex_wake_op = 1;

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  using __gnu_cxx::__int_traits;

namespace
{
  // Return the relative duration from (now_s + now_ns) to (abs_s + abs_ns)
  // as a timespec.
  struct timespec
  relative_timespec(chrono::seconds abs_s, chrono::nanoseconds abs_ns,
		    time_t now_s, long now_ns)
  {
    struct timespec rt;

    // Did we already time out?
    if (now_s > abs_s.count())
      {
	rt.tv_sec = -1;
	return rt;
      }

    const auto rel_s = abs_s.count() - now_s;

    // Convert the absolute timeout to a relative timeout, without overflow.
    if (rel_s > __int_traits<time_t>::__max) [[unlikely]]
      {
	rt.tv_sec = __int_traits<time_t>::__max;
	rt.tv_nsec = 999999999;
      }
    else
      {
	rt.tv_sec = rel_s;
	rt.tv_nsec = abs_ns.count() - now_ns;
	if (rt.tv_nsec < 0)
	  {
	    rt.tv_nsec += 1000000000;
	    --rt.tv_sec;
	  }
      }

    return rt;
  }
} // namespace

  bool
  __atomic_futex_unsigned_base::
  _M_futex_wait_until(unsigned *__addr, unsigned __val, bool __has_timeout,
		      chrono::seconds __s, chrono::nanoseconds __ns)
  {
    if (!__has_timeout)
      {
	// Ignore whether we actually succeeded to block because at worst,
	// we will fall back to spin-waiting.  The only thing we could do
	// here on errors is abort.
	int ret __attribute__((unused));
	ret = syscall (SYS_futex, __addr, futex_wait_op, __val, nullptr);
	__glibcxx_assert(ret == 0 || errno == EINTR || errno == EAGAIN);
	return true;
      }
    else
      {
	struct timeval tv;
	gettimeofday (&tv, NULL);

	// Convert the absolute timeout value to a relative timeout
	auto rt = relative_timespec(__s, __ns, tv.tv_sec, tv.tv_usec * 1000);

	// Did we already time out?
	if (rt.tv_sec < 0)
	  return false;

	if (syscall (SYS_futex, __addr, futex_wait_op, __val, &rt) == -1)
	  {
	    __glibcxx_assert(errno == EINTR || errno == EAGAIN
			     || errno == ETIMEDOUT);
	    if (errno == ETIMEDOUT)
	      return false;
	  }
	return true;
      }
  }

  void
  __atomic_futex_unsigned_base::_M_futex_notify_all(unsigned* __addr)
  {
    // This syscall can fail for various reasons, including in situations
    // in which there is no real error.  Thus, we don't bother checking
    // the error codes.  See the futex documentation and glibc for background.
    syscall (SYS_futex, __addr, futex_wake_op, INT_MAX);
  }

_GLIBCXX_END_NAMESPACE_VERSION
}
#endif // defined(_GLIBCXX_HAVE_LINUX_FUTEX) && ATOMIC_INT_LOCK_FREE > 1
#endif // _GLIBCXX_HAS_GTHREADS
