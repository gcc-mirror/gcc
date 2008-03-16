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
  bool
  test_nanoseconds_is_duration()
  {
    typedef std::nanoseconds D;
    D d;
    D e(d);
    d == e;
    d < e;
    d = e;

    typedef D::tick_type tick_type;
    tick_type t1 = D::ticks_per_second;
    tick_type t2 = D::seconds_per_tick;
    bool b1 = D::is_subsecond;
    tick_type t3 = d.count();
    -d;

    d += e;
    d -= e;
    d *= static_cast<long>(1);
    d /= static_cast<long>(1);

    return b1 || t1 || t2 || t3;
  }
}
