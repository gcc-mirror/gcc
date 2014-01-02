// { dg-options "-std=gnu++0x" }

// Copyright (C) 2011-2014 Free Software Foundation, Inc.
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
#include <new>
#include <testsuite_hooks.h>

struct X
{
  static int counter;
  ~X() { ++counter; }
};

int X::counter = 0;

template<typename T>
struct allocator_with_destroy
{
  typedef T value_type;

  allocator_with_destroy() : called() { }

  void destroy(T* p) { called = true; }

  int called;
};

template<typename T>
struct allocator_without_destroy
{
  typedef T value_type;

  allocator_without_destroy() : called() { }

  int called;
};

void test01()
{
  bool test __attribute__((unused)) = true;

  typedef std::allocator_traits<allocator_with_destroy<X>> traits_type;
  traits_type::allocator_type a;
  X* p = 0;
  traits_type::destroy(a, p);
  VERIFY( a.called );
  VERIFY( X::counter == 0 );
}

void test02()
{
  bool test __attribute__((unused)) = true;

  typedef std::allocator_traits<allocator_without_destroy<X>> traits_type;
  traits_type::allocator_type a;
  char buf[sizeof(X)];
  X* p = ::new (static_cast<void*>(buf)) X();
  traits_type::destroy(a, p);
  VERIFY( !a.called );
  VERIFY( X::counter == 1 );
}

int main()
{
  test01();
  test02();
}
