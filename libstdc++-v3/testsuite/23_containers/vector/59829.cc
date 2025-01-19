// Copyright (C) 2014-2025 Free Software Foundation, Inc.
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

// libstdc++/59829

#include <vector>
#include <testsuite_allocator.h>

// User-defined pointer type that throws if a null pointer is dereferenced.
template<typename T>
struct Pointer : __gnu_test::PointerBase<Pointer<T>, T>
{
  using __gnu_test::PointerBase<Pointer<T>, T>::PointerBase;

  T& operator*() const
  {
    if (!this->value)
      throw "Dereferenced invalid pointer";
    return *this->value;
  }
};

// Minimal allocator using Pointer<T>
template<typename T>
struct Alloc
{
  typedef T value_type;
  typedef Pointer<T> pointer;

  Alloc() = default;
  template<typename U>
    Alloc(const Alloc<U>&) { }

  pointer allocate(std::size_t n)
  { return pointer(std::allocator<T>().allocate(n)); }

  void deallocate(pointer p, std::size_t n)
  { std::allocator<T>().deallocate(p.operator->(), n); }
};

template<typename T>
bool operator==(Alloc<T> l, Alloc<T> r) { return true; }

template<typename T>
bool operator!=(Alloc<T> l, Alloc<T> r) { return false; }

int main()
{
  std::vector<int, Alloc<int>> a;
  (void) a.data();
}
