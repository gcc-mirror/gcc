// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

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

// 20.7.12.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>
#include <functional>
#include <testsuite_hooks.h>

struct A { };

struct D {
    typedef void result_type;
    void operator()(A* p) { delete p;  ++count; }
    int count;
};


// 20.7.12.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Construction from unique_ptr
// See: http://gcc.gnu.org/ml/libstdc++/2008-09/msg00070.html.
int
test01()
{
  D d;
  std::unique_ptr<A, D&> p1(new A, d);

  std::shared_ptr<A> p2(std::move(p1));

  typedef std::reference_wrapper<D> D2;
  D2* p3 = std::get_deleter<D2>(p2);

#if __cpp_rtti
  VERIFY( p3 != 0 );
  VERIFY( &p3->get() == &d );
#else
  VERIFY( p3 == 0 ); // Always returns nullptr without RTTI.
#endif

  return 0;
}

int
main()
{
  test01();
  return 0;
}
