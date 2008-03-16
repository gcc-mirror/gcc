// { dg-do compile }
// { dg-options "-std=gnu++0x" }

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

#include <date_time>

namespace std {
  // duration types
  class nanoseconds;
  class microseconds;
  class milliseconds;
  class seconds;
  class minutes;
  class hours;

  // timepoint type
  class system_time;

  // non-member functions
  system_time get_system_time();

  template<typename Duration>
    system_time operator+(const Duration& td, const system_time& rhs);

  template <class LhsDuration, class RhsDuration>
    bool operator==(const LhsDuration& lhs, const RhsDuration& rhs);
  template <class LhsDuration, class RhsDuration>
    bool operator!=(const LhsDuration& lhs, const RhsDuration& rhs);

  template <class LhsDuration, class RhsDuration>
    bool operator< (const LhsDuration& lhs, const RhsDuration& rhs);
  template <class LhsDuration, class RhsDuration>
    bool operator<=(const LhsDuration& lhs, const RhsDuration& rhs);
  template <class LhsDuration, class RhsDuration>
    bool operator> (const LhsDuration& lhs, const RhsDuration& rhs);
  template <class LhsDuration, class RhsDuration>
    bool operator>=(const LhsDuration& lhs, const RhsDuration& rhs);

/*
  template <class LhsDuration, class RhsDuration>
    FinestDuration operator+(const LhsDuration& lhs, const RhsDuration& rhs);
  template <class LhsDuration, class RhsDuration>
    FinestDuration operator-(const LhsDuration& lhs, const RhsDuration& rhs);
*/
  
  template <class Duration>
    Duration operator*(Duration lhs, long rhs);
  template <class Duration>
    Duration operator*(long lhs, Duration rhs);

  template <class Duration>
    Duration operator/(Duration lhs, long rhs);
}
