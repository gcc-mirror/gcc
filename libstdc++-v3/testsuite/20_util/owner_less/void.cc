// Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <memory>

#if __cplusplus >= 201402L
// The feature-test macro is only defined for C++14 and later.
# if __cpp_lib_transparent_operators < 201510
#  error "__cpp_lib_transparent_operators < 201510"
# endif
#endif

void
test01()
{
  using namespace std;

  static_assert(is_same<owner_less<>, owner_less<void>>::value,
                "owner_less<> uses void specialization");

  shared_ptr<int> sp1;
  shared_ptr<void> sp2;
  shared_ptr<long> sp3;
  weak_ptr<int> wp1;

  owner_less<> cmp;
  cmp(sp1, sp2);
  cmp(sp1, wp1);
  cmp(sp1, sp3);
  cmp(wp1, sp1);
  cmp(wp1, wp1);
}
