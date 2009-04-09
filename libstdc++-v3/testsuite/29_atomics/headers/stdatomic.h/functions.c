// { dg-options "-x c" }
// { dg-do compile }

// Copyright (C) 2008, 2009 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <stdatomic.h>

int main()
{
  volatile atomic_flag f;
  volatile atomic_flag* p = &f;
  memory_order m = memory_order_relaxed;

  // For position only.
  atomic_flag_test_and_set(p);
  atomic_flag_test_and_set_explicit(p, m);
  atomic_flag_clear(p);
  atomic_flag_clear_explicit(p, m);

  return 0;
}
