// { dg-do run { target c++11 } }

// Copyright (C) 2008-2021 Free Software Foundation, Inc.
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

// 20.7.12.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>
#include <testsuite_hooks.h>

struct A { };

struct D {
    typedef void result_type;
    void operator()(A* p) { delete p;  ++count; }
    int count;
};


// 20.7.12.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Construction from unique_ptr
int
test01()
{
  D d = D();
  std::unique_ptr<A, D&> up(new A, d);
  {
      std::shared_ptr<A> sp(std::move(up));
      VERIFY( up.get() == 0 );
      VERIFY( sp.get() != 0 );
      VERIFY( sp.use_count() == 1 );
  }
  VERIFY( d.count == 1 );

  return 0;
}

int
main()
{
  test01();
  return 0;
}
