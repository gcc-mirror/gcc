// { dg-do run { target c++11 } }

// Copyright (C) 2011-2024 Free Software Foundation, Inc.
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

struct X { };

template<typename T>
struct hintable_allocator
{
  typedef T value_type;
  struct const_void_pointer { };
  typedef unsigned size_type;

  hintable_allocator() : called(false) { }

  bool called;

  // this is the overload that should get called:
  T* allocate(size_type n, const_void_pointer) { called = true; return 0; }

  // none of these should get called:
  T* allocate(size_type n);
  T* allocate(size_type n, void*);
  T* allocate(size_type n, const void*);
  T* allocate(size_type n, const_void_pointer) const;
};

void test01()
{
  typedef std::allocator_traits<hintable_allocator<X>> traits_type;
  traits_type::allocator_type a;
  traits_type::const_void_pointer v;
  X* p __attribute__((unused)) = traits_type::allocate(a, 1, v);
  VERIFY( a.called );
}

template<typename T>
struct unhintable_allocator
{
  typedef T value_type;
  typedef unsigned size_type;

  unhintable_allocator() : called(false) { }

  bool called;

  // this is the overload that should get called:
  T* allocate(size_type n) { called = true; return 0; }

  // this should not get called:
  T* allocate(size_type n, void*);
};

void test02()
{
  typedef std::allocator_traits<unhintable_allocator<X>> traits_type;
  traits_type::allocator_type a;
  traits_type::const_void_pointer v;
  X* p __attribute__((unused)) = traits_type::allocate(a, 1, v);
  VERIFY( a.called );
}

int main()
{
  test01();
  test02();
}
