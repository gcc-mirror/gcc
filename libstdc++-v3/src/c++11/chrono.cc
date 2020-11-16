// chrono -*- C++ -*-

// Copyright (C) 2008-2020 Free Software Foundation, Inc.
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

#include <bits/c++config.h>
#include <chrono>

// Conditional inclusion of sys/time.h for gettimeofday
#if !defined(_GLIBCXX_USE_CLOCK_MONOTONIC) && \
    !defined(_GLIBCXX_USE_CLOCK_REALTIME) && \
     defined(_GLIBCXX_USE_GETTIMEOFDAY)
#include <sys/time.h>
#endif

#ifdef _GLIBCXX_USE_CLOCK_GETTIME_SYSCALL
#include <unistd.h>
#include <sys/syscall.h>

# if defined(SYS_clock_gettime_time64) \
  && SYS_clock_gettime_time64 != SYS_clock_gettime
  // Userspace knows about the new time64 syscalls, so it's possible that
  // userspace has also updated timespec to use a 64-bit tv_sec.
  // The SYS_clock_gettime syscall still uses the old definition
  // of timespec where tv_sec is 32 bits, so define a type that matches that.
  struct syscall_timespec { long tv_sec; long tv_nsec; };
# else
  using syscall_timespec = ::timespec;
# endif
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  namespace chrono
  {
    // XXX GLIBCXX_ABI Deprecated
    inline namespace _V2 {

    constexpr bool system_clock::is_steady;

    system_clock::time_point
    system_clock::now() noexcept
    {
#ifdef _GLIBCXX_USE_CLOCK_REALTIME
      // -EINVAL, -EFAULT
#ifdef _GLIBCXX_USE_CLOCK_GETTIME_SYSCALL
      syscall_timespec tp;
      syscall(SYS_clock_gettime, CLOCK_REALTIME, &tp);
#else
      timespec tp;
      clock_gettime(CLOCK_REALTIME, &tp);
#endif
      return time_point(duration(chrono::seconds(tp.tv_sec)
				 + chrono::nanoseconds(tp.tv_nsec)));
#elif defined(_GLIBCXX_USE_GETTIMEOFDAY)
      timeval tv;
      // EINVAL, EFAULT
      gettimeofday(&tv, 0);
      return time_point(duration(chrono::seconds(tv.tv_sec)
				 + chrono::microseconds(tv.tv_usec)));
#else
      std::time_t __sec = std::time(0);
      return system_clock::from_time_t(__sec);
#endif
    }


    constexpr bool steady_clock::is_steady;

    steady_clock::time_point
    steady_clock::now() noexcept
    {
#ifdef _GLIBCXX_USE_CLOCK_MONOTONIC
      // -EINVAL, -EFAULT
#ifdef _GLIBCXX_USE_CLOCK_GETTIME_SYSCALL
      syscall_timespec tp;
      syscall(SYS_clock_gettime, CLOCK_MONOTONIC, &tp);
#else
      timespec tp;
      clock_gettime(CLOCK_MONOTONIC, &tp);
#endif
      return time_point(duration(chrono::seconds(tp.tv_sec)
				 + chrono::nanoseconds(tp.tv_nsec)));
#else
      return time_point(system_clock::now().time_since_epoch());
#endif
    }

  } // end inline namespace _V2
  } // namespace chrono

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
