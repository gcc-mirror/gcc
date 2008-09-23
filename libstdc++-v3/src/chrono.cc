// chrono -*- C++ -*-

// Copyright (C) 2008 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// As a special exception, you may use this file as part of a free software
// library without restriction.  Specifically, if other files instantiate
// templates or use macros or inline functions from this file, or you compile
// this file and link it with other files to produce an executable, this
// file does not by itself cause the resulting executable to be covered by
// the GNU General Public License.  This exception does not however
// invalidate any other reasons why the executable file might be covered by
// the GNU General Public License.

#include <chrono>

#ifdef _GLIBCXX_USE_C99_STDINT_TR1

// conditional inclusion of sys/time.h for gettimeofday
#if !defined(_GLIBCXX_USE_CLOCK_MONOTONIC) && \
    !defined(_GLIBCXX_USE_CLOCK_REALTIME) && \
     defined(_GLIBCXX_USE_GETTIMEOFDAY)
#include <sys/time.h>
#endif

namespace std
{
  namespace chrono
  {
    const bool system_clock::is_monotonic;

    system_clock::time_point
    system_clock::now()
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
      gettimeofday(&tv, NULL);
      return time_point(duration(chrono::seconds(tv.tv_sec)
				 + chrono::microseconds(tv.tv_usec)));
#else
      std::time_t __sec = std::time(0);
      return system_clock::from_time_t(__sec);
#endif
    }
    
#ifdef _GLIBCXX_USE_CLOCK_MONOTONIC
    const bool monotonic_clock::is_monotonic;
    
    monotonic_clock::time_point
    monotonic_clock::now()
    {
      timespec tp;
      // -EINVAL, -EFAULT
      clock_gettime(CLOCK_MONOTONIC, &tp);
      return time_point(duration(chrono::seconds(tp.tv_sec)
				 + chrono::nanoseconds(tp.tv_nsec)));
    }
#endif
  }
}

#endif // _GLIBCXX_USE_C99_STDINT_TR1
