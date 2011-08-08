// chrono -*- C++ -*-

// Copyright (C) 2008, 2009, 2010, 2011 Free Software Foundation, Inc.
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

#include <chrono>

#ifdef _GLIBCXX_USE_C99_STDINT_TR1

// conditional inclusion of sys/time.h for gettimeofday
#if !defined(_GLIBCXX_USE_CLOCK_MONOTONIC) && \
    !defined(_GLIBCXX_USE_CLOCK_REALTIME) && \
     defined(_GLIBCXX_USE_GETTIMEOFDAY)
#include <sys/time.h>
#endif

namespace std _GLIBCXX_VISIBILITY(default)
{
  namespace chrono
  {
  _GLIBCXX_BEGIN_NAMESPACE_VERSION
 
    constexpr bool system_clock::is_steady;

    system_clock::time_point
    system_clock::now() throw ()
    {
#ifdef _GLIBCXX_USE_CLOCK_REALTIME
      timespec tp;
      // -EINVAL, -EFAULT
      clock_gettime(CLOCK_REALTIME, &tp);
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
    
#ifdef _GLIBCXX_USE_CLOCK_MONOTONIC
    constexpr bool steady_clock::is_steady;
    
    steady_clock::time_point
    steady_clock::now()
    {
      timespec tp;
      // -EINVAL, -EFAULT
      clock_gettime(CLOCK_MONOTONIC, &tp);
      return time_point(duration(chrono::seconds(tp.tv_sec)
				 + chrono::nanoseconds(tp.tv_nsec)));
    }
#endif

  _GLIBCXX_END_NAMESPACE_VERSION
  } // namespace chrono
} // namespace std

#endif // _GLIBCXX_USE_C99_STDINT_TR1
