// { dg-do run { target c++11 } }

// Copyright (C) 2014-2017 Free Software Foundation, Inc.
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
#include <testsuite_allocator.h>

// User-defined pointer type with non-trivial destructor.
template<typename T>
struct Pointer : __gnu_test::PointerBase<Pointer<T>, T>
{
  using __gnu_test::PointerBase<Pointer<T>, T>::PointerBase;

  ~Pointer() { /* non-trivial */ }
};

// Minimal allocator with user-defined pointer type.
template<typename T>
struct Alloc
{
  typedef T value_type;
  typedef Pointer<T> pointer;

  Alloc() = default;

  template<typename U>
    Alloc(const Alloc<U>&) { }

  pointer allocate(std::size_t n)  // does not take a hint
  { return pointer(std::allocator<T>().allocate(n)); }

  void deallocate(pointer p, std::size_t n)
  { std::allocator<T>().deallocate(p.value, n); }
};

template<typename T>
bool operator==(Alloc<T> l, Alloc<T> r) { return true; }

template<typename T>
bool operator!=(Alloc<T> l, Alloc<T> r) { return false; }

void test01()
{
  typedef std::allocator_traits<Alloc<int>> traits_type;
  traits_type::allocator_type a;
  traits_type::const_void_pointer v;
  traits_type::pointer p = traits_type::allocate(a, 1, v);
  traits_type::deallocate(a, p, 1);
}

int main()
{
  test01();
}
