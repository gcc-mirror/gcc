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

#include <date_time>

namespace std
{
  system_time
  get_system_time()
  {
    std::time_t sec = std::time(0);
    return system_time(sec);
  }

  const nanoseconds::tick_type nanoseconds::ticks_per_second;
  const nanoseconds::tick_type nanoseconds::seconds_per_tick;
  const bool nanoseconds::is_subsecond;

  const system_time::tick_type system_time::ticks_per_second;
  const system_time::tick_type system_time::seconds_per_tick;
  const bool system_time::is_subsecond;
}
