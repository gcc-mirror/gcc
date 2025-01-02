// Compatibility symbols for previous versions, C++0x bits -*- C++ -*-

// Copyright (C) 2013-2025 Free Software Foundation, Inc.
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

#if _GLIBCXX_INLINE_VERSION
# error "compatibility-thread-c++0x.cc is not needed for gnu-versioned-namespace"
#endif

#ifdef _GLIBCXX_USE_C99_STDINT_TR1

#ifdef _GLIBCXX_USE_GETTIMEOFDAY
#include <sys/time.h>
#endif

#define system_clock system_clockXX
#define steady_clock steady_clockXX
#include <chrono>
#undef system_clock
#undef steady_clock

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  namespace chrono
  {
    // NB: Default configuration was no realtime.
    struct system_clock
    {
#ifdef _GLIBCXX_USE_GETTIMEOFDAY
      typedef chrono::microseconds    				duration;
#else
      typedef chrono::seconds	      				duration;
#endif

      typedef duration::rep    					rep;
      typedef duration::period 					period;
      typedef chrono::time_point<system_clock, duration> 	time_point;

      static_assert(system_clock::duration::min()
		    < system_clock::duration::zero(),
		    "a clock's minimum duration cannot be less than its epoch");

      static constexpr bool is_steady = false;

      static time_point
      now() noexcept;
    };

    constexpr bool system_clock::is_steady;

    system_clock::time_point
    system_clock::now() noexcept
    {
#ifdef _GLIBCXX_USE_GETTIMEOFDAY
      timeval tv;
      // EINVAL, EFAULT
      gettimeofday(&tv, 0);
      return time_point(duration(chrono::seconds(tv.tv_sec)
				 + chrono::microseconds(tv.tv_usec)));
#else
      std::time_t __sec = std::time(0);
      // This is the conversion done by system_clock::from_time_t(__sec)
      typedef chrono::time_point<system_clock, seconds>	__from;
      return time_point_cast<system_clock::duration>
	     (__from(chrono::seconds(__sec)));
#endif
    }
  } // namespace chrono

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // _GLIBCXX_USE_C99_STDINT_TR1
