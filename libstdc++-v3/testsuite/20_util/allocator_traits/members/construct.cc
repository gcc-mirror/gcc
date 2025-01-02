// { dg-do run { target c++11 } }

// Copyright (C) 2011-2025 Free Software Foundation, Inc.
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
#include <cstddef>
#include <testsuite_hooks.h>

struct X
{
  static int counter;
  X() { }
  X(const X&) { ++counter; }
  explicit X(int) { ++counter; }
  X(int, int) { ++counter; }
  X(int, int, int) { ++counter; }
};

int X::counter = 0;

template<typename T>
struct fake_allocator
{
  typedef T value_type;

  fake_allocator() : counter() {}

  int counter;

  T* allocate(std::size_t n) { return (T*)new char[n*sizeof(T)]; }
  void deallocate(T* p, std::size_t) { delete[] (char*)p; }

  // don't actually construct anything when these are called
  void construct(T* p) { ++counter; }
  void construct(T* p, int, int) { ++counter; }
};

void test01()
{
  typedef std::allocator_traits<fake_allocator<X>> traits_type;
  traits_type::allocator_type a;
  X* p = traits_type::allocate(a, 1);
  traits_type::construct(a, p);
  VERIFY( a.counter == 1 );
  traits_type::construct(a, p, 1);
  VERIFY( a.counter == 1 );
  VERIFY( X::counter == 1 );
  traits_type::destroy(a, p);
  traits_type::construct(a, p, 1, 1);
  VERIFY( a.counter == 2 );
  VERIFY( X::counter == 1 );
  traits_type::construct(a, p, 1, 1, 1);
  VERIFY( a.counter == 2 );
  VERIFY( X::counter == 2 );
  traits_type::destroy(a, p);
  traits_type::deallocate(a, p, 1);
}

int main()
{
  test01();
}
