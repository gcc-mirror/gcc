// { dg-do run { target c++11 } }

// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_hooks.h>

struct A
{
  int i = 0;
};

// C++17 20.11.2.2.5 shared_ptr observers [util.smartptr.shared.obs]

// get
void
test01()
{
  A * const a = new A[2];
  const std::shared_ptr<A[2]> p(a);
  VERIFY( p.get() == a );
}

// get
void
test02()
{
  A * const a = new A[2];
  const std::shared_ptr<A[]> p(a);
  VERIFY( p.get() == a );
}

// operator[]
void
test03()
{
  A * const a = new A[2];
  const std::shared_ptr<A[2]> p(a);
  VERIFY( &p[0] == a );
}

// operator[]
void
test04()
{
  A * const a = new A[2];
  const std::shared_ptr<A[]> p(a);
  VERIFY( &p[0] == a );
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
}
