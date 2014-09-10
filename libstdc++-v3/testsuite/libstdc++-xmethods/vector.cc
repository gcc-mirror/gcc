// { dg-do run }
// { dg-options "-g -O0" }

// Copyright (C) 2014 Free Software Foundation, Inc.
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
  std::vector<int> v;
  v.push_back(1);
  v.push_back(2);
  v.push_back(3);
// { dg-final { note-test v\[0\] 1 } }
// { dg-final { note-test v\[1\] 2 } }
// { dg-final { note-test v\[2\] 3 } }
// { dg-final { note-test v.size() 3 } }

  return 0;  // Mark SPOT
}

// { dg-final { gdb-test SPOT {} 1 } }
