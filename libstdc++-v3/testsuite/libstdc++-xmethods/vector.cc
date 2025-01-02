// { dg-do run }
// { dg-options "-g -O0" }

// Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

#include <vector>

int
main ()
{
  std::vector<int> v0, v1;
  std::vector<bool> bv0, bv1, bv2, bv3;

  v1.push_back (1);
  v1.push_back (2);
  v1.push_back (3);

  for (int i = 0; i < 15; i++)
    bv1.push_back (i % 3);

  for (int i = 0; i < 64; i++)
    bv2.push_back (i % 2);

  for (int i = 0; i < 65; i++)
    bv3.push_back (i % 2);

// { dg-final { note-test v1\[0\] 1 } }
// { dg-final { note-test v1\[1\] 2 } }
// { dg-final { note-test v1\[2\] 3 } }
// { dg-final { note-test bv1\[0\] false } }
// { dg-final { note-test bv1\[1\] true } }
// { dg-final { note-test bv1\[14\] true } }
// { dg-final { note-test bv2\[0\] false } }
// { dg-final { note-test bv2\[1\] true } }
// { dg-final { note-test bv2\[63\] true } }
// { dg-final { note-test bv3\[0\] false } }
// { dg-final { note-test bv3\[1\] true } }
// { dg-final { note-test bv3\[63\] true } }
// { dg-final { note-test v0.size() 0 } }
// { dg-final { note-test bv0.size() 0 } }
// { dg-final { note-test v1.size() 3 } }
// { dg-final { note-test bv1.size() 15 } }
// { dg-final { note-test bv2.size() 64 } }
// { dg-final { note-test bv3.size() 65 } }
// { dg-final { note-test v0.empty() true } }
// { dg-final { note-test v1.empty() false } }
// { dg-final { note-test bv0.empty() true } }
// { dg-final { note-test bv1.empty() false } }
// { dg-final { note-test bv2.empty() false } }
// { dg-final { note-test bv3.empty() false } }
// { dg-final { note-test v1.front() 1 } }
// { dg-final { note-test v1.back() 3 } }
// { dg-final { note-test bv1.front() false } }
// { dg-final { note-test bv1.back() true } }
// { dg-final { note-test bv2.front() false } }
// { dg-final { note-test bv2.back() true } }
// { dg-final { note-test bv3.front() false } }
// { dg-final { note-test bv3.back() false } }
// { dg-final { note-test v1.at(1) 2 } }
// { dg-final { note-test bv1.at(0) false } }
// { dg-final { note-test bv1.at(1) true } }
// { dg-final { note-test bv1.at(14) true } }
// { dg-final { note-test bv2.at(0) false } }
// { dg-final { note-test bv2.at(1) true } }
// { dg-final { note-test bv2.at(63) true } }
// { dg-final { note-test bv3.at(0) false } }
// { dg-final { note-test bv3.at(1) true } }
// { dg-final { note-test bv3.at(63) true } }
// { dg-final { note-test bv3.at(64) false } }

// { dg-final { whatis-test v0.empty() bool } }
// { dg-final { whatis-test v0.size() std::size_t } }
// { dg-final { whatis-test v1.front() int } }
// { dg-final { whatis-test v1.back() int } }
// { dg-final { whatis-test v1\[0\] int } }
// { dg-final { whatis-test v1.at(1) int } }
// { dg-final { whatis-test bv0.empty() bool } }
// { dg-final { whatis-test bv0.size() std::size_t } }
// { dg-final { whatis-test bv1.front() bool } }
// { dg-final { whatis-test bv1.back() bool } }
// { dg-final { whatis-test bv1\[0\] bool } }
// { dg-final { whatis-test bv1.at(1) bool } }

  return 0;  // Mark SPOT
}

// { dg-final { gdb-test SPOT {} 1 } }
