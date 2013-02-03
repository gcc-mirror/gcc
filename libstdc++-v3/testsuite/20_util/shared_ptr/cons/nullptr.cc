// { dg-options "-std=gnu++0x" }

// Copyright (C) 2010-2013 Free Software Foundation, Inc.
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

// 20.9.11.2 Class template shared_ptr [util.smartptr.shared]

#include <memory>
#include <cstddef>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

// 20.9.11.2.1 shared_ptr constructors [util.smartptr.shared.const]

// Construction from nullptr

struct deleter
{
  int count;
  deleter() : count(0) { }
  void operator()(std::nullptr_t) { ++count; }
  void operator()(int*) const { throw "wrong type passed to deleter"; }
};

void
test01()
{
  bool test __attribute__((unused)) = true;

  std::shared_ptr<int> p = nullptr;
  VERIFY( p.get() == nullptr );
  VERIFY( p.use_count() == 0 );

}

void
test02()
{
  bool test __attribute__((unused)) = true;

  deleter d;
  std::shared_ptr<int> p(nullptr, std::ref(d));
  VERIFY( p.get() == nullptr );
  VERIFY( p.use_count() == 1 );

  p = nullptr;
  VERIFY( p.use_count() == 0 );
  VERIFY( d.count == 1 );
}


void
test03()
{
  bool test __attribute__((unused)) = true;

  deleter d;
  __gnu_test::tracker_allocator<int> a;
  std::shared_ptr<int> p(nullptr, std::ref(d), a);
  VERIFY( p.get() == nullptr );
  VERIFY( p.use_count() == 1 );

  p = nullptr;
  VERIFY( p.use_count() == 0 );
  VERIFY( d.count == 1 );

  typedef __gnu_test::tracker_allocator_counter c;
  VERIFY( c::get_destruct_count() == c::get_construct_count() );
  VERIFY( c::get_deallocation_count() == c::get_allocation_count() );
}

int
main()
{
  test01();
  test02();
  test03();
  return 0;
}
