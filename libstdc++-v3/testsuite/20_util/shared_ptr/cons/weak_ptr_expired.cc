// { dg-do run { target c++11 } }

// Copyright (C) 2005-2024 Free Software Foundation, Inc.
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

// 20.6.6.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>
#include <string>
#include <testsuite_hooks.h>

struct A { };

// 20.6.6.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Construction from expired weak_ptr
int
test01()
{
  bool test = false;

  std::shared_ptr<A> a1(new A);
  std::weak_ptr<A> wa(a1);
  a1.reset();
  VERIFY( wa.expired() );
  try
  {
    std::shared_ptr<A> a2(wa);
  }
  catch (const std::bad_weak_ptr& e)
  {
    // Expected.
    if (e.what() == std::string("bad_weak_ptr"))
      test = true;
  }
  VERIFY( test );

  return 0;
}

int
main()
{
  test01();
  return 0;
}
