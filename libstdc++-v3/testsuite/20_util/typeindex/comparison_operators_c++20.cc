// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

// Copyright (C) 2020-2021 Free Software Foundation, Inc.
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

#include <typeindex>
#include <testsuite_hooks.h>

void test01()
{
  class Abraca { };
  Abraca a1, a2_;
  const Abraca a2 = a2_;

  const std::type_index arr[] = {
    typeid(int), typeid(double), typeid(Abraca), typeid(const Abraca),
    typeid(const Abraca&), typeid(a1), typeid(a2)
  };

  for (const std::type_index& t1 : arr)
    for (const std::type_index& t2 : arr)
    {
      VERIFY( (t1 == t2) == std::is_eq(t1 <=> t2) );
      VERIFY( (t1 != t2) == std::is_neq(t1 <=> t2) );
      VERIFY( (t1 < t2) == std::is_lt(t1 <=> t2) );
      VERIFY( (t1 > t2) == std::is_gt(t1 <=> t2) );
      VERIFY( (t1 <= t2) == std::is_lteq(t1 <=> t2) );
      VERIFY( (t1 >= t2) == std::is_gteq(t1 <=> t2) );
    }
}

int main()
{
  test01();
}
