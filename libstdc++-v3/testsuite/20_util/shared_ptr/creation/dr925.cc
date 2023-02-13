// { dg-options "-Wno-deprecated" }
// { dg-add-options using-deprecated }
// { dg-do run { target c++11 } }
// { dg-require-effective-target hosted }

// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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

// 20.9.11.2 Template class shared_ptr [util.smartptr.shared]

#include <memory>
#include <testsuite_hooks.h>

struct A
{
};

std::unique_ptr<A>
create_unique_ptr()
{
  return std::unique_ptr<A>(new A());
}

std::auto_ptr<A>
create_auto_ptr()
{
  return std::auto_ptr<A>(new A());
}

void
process(std::shared_ptr<A> a)
{
  VERIFY( a.get() != 0 );
  VERIFY( a.use_count() == 1 );
}

// 20.9.11.2.1 shared_ptr creation [util.smartptr.shared.const]

// Implicit conversion of auto_ptr to shared_ptr is allowed

void
test01()
{
  process(create_auto_ptr());
}

void
test02()
{
  std::auto_ptr<A> a = create_auto_ptr();
  process(std::move(a));
}

// Implicit conversion of unique_ptr to shared_ptr is allowed

void
test03()
{
  process(create_unique_ptr());
}

void
test04()
{
  std::unique_ptr<A> a = create_unique_ptr();
  process(std::move(a));
}

int
main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
