// Copyright (C) 2017 Free Software Foundation, Inc.
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

// { dg-do run { target c++11 } }
// { dg-options "-O0" }
// { dg-xfail-run-if "PR c++/65816" { *-*-* } }

#include <list>
#include <testsuite_hooks.h>
#include <testsuite_allocator.h>

#include <ext/aligned_buffer.h>

using T = int;

using __gnu_test::default_init_allocator;

void test01()
{
  typedef default_init_allocator<T> alloc_type;
  typedef std::list<T, alloc_type> test_type;

  __gnu_cxx::__aligned_buffer<test_type> buf;
  __builtin_memset(buf._M_addr(), ~0, sizeof(test_type));

  VERIFY( buf._M_ptr()->get_allocator().state != 0 );

  test_type *tmp = ::new(buf._M_addr()) test_type();

  VERIFY( tmp->get_allocator().state == 0 );

  tmp->~test_type();
}

void test02()
{
  typedef default_init_allocator<T> alloc_type;
  typedef std::list<T, alloc_type> test_type;

  __gnu_cxx::__aligned_buffer<test_type> buf;
  __builtin_memset(buf._M_addr(), ~0, sizeof(test_type));

  VERIFY( buf._M_ptr()->get_allocator().state != 0 );

  test_type *tmp = ::new(buf._M_addr()) test_type;

  VERIFY( tmp->get_allocator().state == 0 );

  tmp->~test_type();
}

int main()
{
  test01();
  test02();
  return 0;
}
