// std::chrono::tai_clock, gps_clock

// Copyright (C) 2021-2026 Free Software Foundation, Inc.
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

//
// ISO C++ 14882:2020
// 27.7.4 [time.clock.tai], 27.7.5 [time.clock.gps]
// P0355R7

#include <chrono>

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

namespace chrono
{
#if defined __glibcxx_chrono_cxx20 && _GLIBCXX_HOSTED
  // TODO use CLOCK_TAI on linux, add extension point.
  time_point<tai_clock>
  tai_clock::now()
  { return from_utc(utc_clock::now()); }

  // TODO add extension point.
  time_point<gps_clock>
  gps_clock::now()
  { return from_utc(utc_clock::now()); }
#endif
}

_GLIBCXX_END_NAMESPACE_VERSION
}
