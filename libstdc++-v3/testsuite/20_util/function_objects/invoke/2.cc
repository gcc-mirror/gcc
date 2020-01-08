// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <functional>
#include <testsuite_hooks.h>

constexpr int sq(int i) { return i * i; }

template<typename Val, typename Expected>
bool chk(Val&& val, Expected&& exp)
{
  return std::is_same<Val, Expected>::value && val == exp;
}

#define VERIFY_T(x,y) VERIFY(chk(x,y))

void
test01()
{
  VERIFY_T( std::__invoke(sq, 2), 4 );
  VERIFY_T( std::__invoke_r<int>(sq, 3), 9 );
  VERIFY_T( std::__invoke_r<char>(sq, 4), '\x10' );
}

int main()
{
  test01();
}
