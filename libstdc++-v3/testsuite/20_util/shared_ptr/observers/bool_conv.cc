// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2005-2025 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

struct A { };

// 20.6.6.2.5 shared_ptr observers [util.smartptr.shared.obs]

// conversion to bool
void
test01()
{
  const std::shared_ptr<A> p1;
  VERIFY( static_cast<bool>(p1) == false );
  const std::shared_ptr<A> p2(p1);
  VERIFY( static_cast<bool>(p2) == false );
}

void
test02()
{
  std::shared_ptr<A> p1(new A);
  VERIFY( static_cast<bool>(p1) );
  std::shared_ptr<A> p2(p1);
  VERIFY( static_cast<bool>(p2) );
  p1.reset();
  VERIFY( !static_cast<bool>(p1) );
  VERIFY( static_cast<bool>(p2) );
}

void
test03()
{
  std::shared_ptr<A> p1(new A);
  std::shared_ptr<A> p2(p1);
  p2.reset(new A);
  VERIFY( static_cast<bool>(p1) );
  VERIFY( static_cast<bool>(p2) );
}


int 
main()
{
  test01();
  test02();
  test03();
  return 0;
}
