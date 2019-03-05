// { dg-do run { target c++11 } }
// { dg-options "-g -O0" }

// Copyright (C) 2014-2019 Free Software Foundation, Inc.
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

#include <array>

int
main ()
{
  std::array<int, 10> a;
  std::array<int, 0> a1;

  for (int i = 0; i < 10; i++)
    a[i] = 100 + i;

// { dg-final { note-test a.size() 10 } }
// { dg-final { note-test a.empty() false } }
// { dg-final { note-test a1.empty() true } }
// { dg-final { note-test a1.size() 0 } }
// { dg-final { note-test a.front() 100 } }
// { dg-final { note-test a.back() 109 } }
// { dg-final { note-test a.at(5) 105 } }
// { dg-final { note-test a\[0\] 100 } }
// { dg-final { note-test a\[4\] 104 } }
// { dg-final { note-test a\[9\] 109 } }

// { dg-final { whatis-test a.size() std::size_t } }
// { dg-final { whatis-test a.empty() bool } }
// { dg-final { whatis-test a.front() int } }
// { dg-final { whatis-test a.back() int } }
// { dg-final { whatis-test a.at(5) int } }
// { dg-final { whatis-test a\[0\] int } }

  return 0;  // Mark SPOT
}

// { dg-final { gdb-test SPOT {} 1 } }
