// 20010613 gdr

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

// This is DR-253.  Test for accessible assignment-operators.
#include <valarray>
#include <testsuite_hooks.h>

int main()
{
  using std::valarray;
  using std::slice;
  valarray<int> v(1, 10), w(2, 10);

  w[slice(0, 3, 3)] = v[slice(2, 3, 3)];

  VERIFY(v[0] == 1 && w[0] == 1);
  VERIFY(v[3] == 1 && w[3] == 1);
  VERIFY(v[6] == 1 && w[6] == 1);

  std::slice_array<int> t __attribute__((unused)) = v[slice(0, 10, 1)];
  
  return 0;
}
