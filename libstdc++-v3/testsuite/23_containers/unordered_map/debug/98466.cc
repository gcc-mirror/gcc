// Copyright (C) 2021-2025 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }

#include <debug/unordered_map>
#include <testsuite_hooks.h>

// PR libstdc++/98466

void test01()
{
  __gnu_debug::unordered_map<int, int>::iterator it{};
  VERIFY( it == it );

  __gnu_debug::unordered_map<int, int>::const_iterator cit{};
  VERIFY( cit == cit );

  __gnu_debug::unordered_map<int, int>::local_iterator lit{};
  VERIFY( lit == lit );

  __gnu_debug::unordered_map<int, int>::const_local_iterator clit{};
  VERIFY( clit == clit );
}

int main()
{
  test01();
  return 0;
}
