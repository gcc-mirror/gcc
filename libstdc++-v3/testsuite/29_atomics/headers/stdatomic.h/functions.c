// { dg-options "-x c" }
// { dg-do compile }

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
  atomic_flag_fence(p, m);

  return 0;
}
