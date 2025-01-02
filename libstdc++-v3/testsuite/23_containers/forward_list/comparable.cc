// { dg-do run { target c++11 } }

// Copyright (C) 2008-2025 Free Software Foundation, Inc.
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

#include <forward_list>
#include <testsuite_hooks.h>

void
test01()
{
  std::forward_list<double> a = {0.0, 1.0, 2.0, 3.0, 4.0};
  std::forward_list<double> b = {0.0, 1.0, 2.0, 3.0, 4.0, 5.0};

  VERIFY((a == b) == false);
  VERIFY((a <  b) == true);
  VERIFY((a != b) == true);
  VERIFY((a >  b) == false);
  VERIFY((a >= b) == false);
  VERIFY((a <= b) == true);

  VERIFY((b == a) == false);
  VERIFY((b <  a) == false);
  VERIFY((b != a) == true);
  VERIFY((b >  a) == true);
  VERIFY((b >= a) == true);
  VERIFY((b <= a) == false);
}

void
test02()
{
  // The EqualityComparable requirements only require ==
  struct X {
    bool operator==(const X&) const { return true; }
  };

  std::forward_list<X> a(2);
  const auto b = a;
  VERIFY( a == b );
}

void
test03()
{
  // The LessThanComparable requirements only require <
  struct X {
    bool operator<(const X&) const { return false; }
  };

  std::forward_list<X> a(2);
  const auto b = a;
  VERIFY( !(a < b) );
  VERIFY( !(a > b) );
  VERIFY( a <= b );
  VERIFY( a >= b );
}

int main()
{
  test01();
  test02();
  test03();
}
