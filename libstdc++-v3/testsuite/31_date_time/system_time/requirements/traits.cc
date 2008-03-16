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

namespace gnu
{
  void
  test_system_time_traits()
  {
    static_assert(std::nanoseconds::ticks_per_second == std::nanoseconds::ticks_per_second, "FIXME");
    static_assert(std::system_time::seconds_per_tick == 0, "FIXME");
    static_assert(std::system_time::is_subsecond == true, "FIXME");
  }
}
