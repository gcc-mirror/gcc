// { dg-do run { target c++11 } }

// Copyright (C) 2015-2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.


#include <functional>
#include <testsuite_hooks.h>

using namespace std;

enum E1 : int {FIRST=1, SECOND=2};
enum class E2 : int {THIRD=42, FOURTH=666};

int main()
{
  VERIFY(hash<int>{}(1) == hash<E1>{}(FIRST));
  VERIFY(hash<int>{}(2) == hash<E1>{}(SECOND));
  VERIFY(hash<int>{}(42) == hash<E2>{}(E2::THIRD));
  VERIFY(hash<int>{}(666) == hash<E2>{}(E2::FOURTH));
}
