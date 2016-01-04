// futex -*- C++ -*-

// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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
#if defined(_GLIBCXX_HAS_GTHREADS) && defined(_GLIBCXX_USE_C99_STDINT_TR1)
#if defined(_GLIBCXX_HAVE_LINUX_FUTEX) && ATOMIC_INT_LOCK_FREE > 1
#include <chrono>
#include <climits>
#include <syscall.h>
#include <unistd.h>
#include <sys/time.h>
#include <errno.h>
#include <debug/debug.h>

// Constants for the wait/wake futex syscall operations
const unsigned futex_wait_op = 0;
const unsigned futex_wake_op = 1;

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  bool
  __atomic_futex_unsigned_base::_M_futex_wait_until(unsigned *__addr,
      unsigned __val,
      bool __has_timeout, chrono::seconds __s, chrono::nanoseconds __ns)
  {
    if (!__has_timeout)
      {
	// Ignore whether we actually succeeded to block because at worst,
	// we will fall back to spin-waiting.  The only thing we could do
	// here on errors is abort.
	int ret __attribute__((unused));
	ret = syscall (SYS_futex, __addr, futex_wait_op, __val, nullptr);
	_GLIBCXX_DEBUG_ASSERT(ret == 0 || errno == EINTR || errno == EAGAIN);
	return true;
      }
    else
      {
	struct timeval tv;
	gettimeofday (&tv, NULL);
	// Convert the absolute timeout value to a relative timeout
	struct timespec rt;
	rt.tv_sec = __s.count() - tv.tv_sec;
	rt.tv_nsec = __ns.count() - tv.tv_usec * 1000;
	if (rt.tv_nsec < 0)
	  {
	    rt.tv_nsec += 1000000000;
	    --rt.tv_sec;
	  }
	// Did we already time out?
	if (rt.tv_sec < 0)
	  return false;

	if (syscall (SYS_futex, __addr, futex_wait_op, __val, &rt) == -1)
	  {
	    _GLIBCXX_DEBUG_ASSERT(errno == EINTR || errno == EAGAIN
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
#endif
#endif
