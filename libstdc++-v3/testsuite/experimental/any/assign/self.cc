// Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

// { dg-do run { target c++14 } }

#include <experimental/any>
#include <set>
#include <testsuite_hooks.h>

std::set<const void*> live_objects;

struct A {
  A() { live_objects.insert(this); }
  ~A() { live_objects.erase(this); }
  A(const A& a) { VERIFY(live_objects.count(&a)); live_objects.insert(this); }
};

void
test01()
{
  using std::experimental::any;

  any a;
  a = a;
  VERIFY( a.empty() );

  a = A{};
  a = a;
  VERIFY( !a.empty() );

  a.clear();
  VERIFY( live_objects.empty() );
}

void
test02()
{
  using std::experimental::any;

  struct X {
    any a;
  };

  X x;
  std::swap(x, x); // results in "self-move-assignment" of X::a
  VERIFY( x.a.empty() );

  x.a = A{};
  std::swap(x, x); // results in "self-move-assignment" of X::a
  VERIFY( !x.a.empty() );

  x.a.clear();
  VERIFY( live_objects.empty() );
}

void
test03()
{
  using std::experimental::any;

  any a;
  a.swap(a);
  VERIFY( a.empty() );

  a = A{};
  a.swap(a);
  VERIFY( !a.empty() );

  a.clear();
  VERIFY( live_objects.empty() );
}

int
main()
{
  test01();
  test02();
  test03();
}
