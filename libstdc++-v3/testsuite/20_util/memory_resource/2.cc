// { dg-do run { target c++17 } }
// { dg-skip-if "" { *-*-* } { -fno-aligned-new } }

// Copyright (C) 2018-2024 Free Software Foundation, Inc.
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

#include <memory_resource>
#include <testsuite_allocator.h>

struct R : std::pmr::memory_resource {
  void* do_allocate(std::size_t, std::size_t) override;
  void do_deallocate(void*, std::size_t, std::size_t) override;
  bool do_is_equal(const std::pmr::memory_resource&) const noexcept override;
};

bool called = false;

void* R::do_allocate(std::size_t bytes, std::size_t a)
{
  called = true;
  return ::operator new(bytes, std::align_val_t(a));
}

void R::do_deallocate(void* p, std::size_t bytes, std::size_t a)
{
  called = true;
  ::operator delete(p, bytes, std::align_val_t(a));
}

bool R::do_is_equal(const std::pmr::memory_resource& r) const noexcept
{
  called = true;
  return this == &r;
}

void
test01()
{
  R res;
  called = false;
  auto p = res.allocate(1, 1);
  VERIFY( called );
  called = false;
  res.deallocate(p, 1, 1);
  VERIFY( called );
  called = false;
  VERIFY( res == res );
  VERIFY( !called );
  VERIFY( ! (res != res) );
  VERIFY( !called );

  struct X { int i = 0; };
  struct MultipleInheritance : X, R { };
  MultipleInheritance m;
  VERIFY( m == m );
  VERIFY( !called );
  VERIFY( ! (m != m) );
  VERIFY( !called );
  VERIFY( m.is_equal(m) );
  VERIFY( called );
  called = false;
  VERIFY( ! (m == res) );
  VERIFY( called );
  called = false;
  VERIFY( m != res );
  VERIFY( called );
  called = false;
  VERIFY( ! (res == m) );
  VERIFY( called );
  called = false;
  VERIFY( res != m );
  VERIFY( called );
  called = false;
}

void
test02()
{
  __gnu_test::memory_resource r1, r2;
  VERIFY( r1 == r1 );
  VERIFY( ! (r1 != r1) );
  VERIFY( r1.is_equal(r1) );
  VERIFY( r2 == r2 );
  VERIFY( r2.is_equal(r2) );
  VERIFY( ! (r1 == r2) );
  VERIFY( r1 != r2 );
  VERIFY( ! r1.is_equal(r2) );
  VERIFY( ! (r2 == r1) );
  VERIFY( r2 != r1 );
  VERIFY( ! r2.is_equal(r1) );
}

int main()
{
  test01();
  test02();
}
