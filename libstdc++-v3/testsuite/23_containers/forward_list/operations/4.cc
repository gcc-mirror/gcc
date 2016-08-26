// { dg-do run { target c++11 } }

// Copyright (C) 2008-2016 Free Software Foundation, Inc.
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

// 23.2.3.n forward_list xxx [lib.forward_list.xxx]

#include <forward_list>
#include <testsuite_hooks.h>

bool test __attribute__((unused)) = true;

// This test verifies the following:
//   unique
void
test01()
{
  std::forward_list<int> fl = {99, 5, 99, 6, -5, 666, 777, 888,
                               42, 42, 42, 42, 42, 7, 0, 0, 0, 9, 9, 9};

  fl.unique();

  std::forward_list<int> fl2 = {99, 5, 99, 6, -5, 666, 777, 888, 42, 7, 0, 9};
  VERIFY(fl == fl2);
}

//  Test comparison predicate.
template<typename Num>
  class Mod
  {
  public:
    Mod(const Num & mod)
      {
        m = mod;
      }
    bool operator()(const Num i, const Num j)
      {
        return i%m == j%m;
      }
  private:
    Num m;
  };

// This test verifies the following:
//   unique with predicate
void
test02()
{
  std::forward_list<int> fl = {99, 5, 99, 6, -5, 666, 777, 888, 42, 7, 0, 9};

  fl.unique(Mod<int>(111));

  std::forward_list<int> fl2 = {99, 5, 99, 6, -5, 666, 42, 7, 0, 9};
  VERIFY(fl == fl2);
}

int
main()
{
  test01();
  test02();
  return 0;
}
