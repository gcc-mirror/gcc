// Copyright (C) 2014-2018 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }

#include <deque>

template<typename T>
struct Alloc
{
  using value_type = T;

  Alloc() = default;

  template<typename U>
    Alloc(const Alloc<U>&) { }

  Alloc& operator=(const Alloc&) = delete;

  T* allocate(std::size_t n)
  { return std::allocator<T>{}.allocate(n); }

  void deallocate(T* p, std::size_t n)
  { std::allocator<T>{}.deallocate(p, n); }
};

template<typename T>
bool operator==(const Alloc<T>&, const Alloc<T>&) { return true; }

template<typename T>
bool operator!=(const Alloc<T>&, const Alloc<T>&) { return false; }

void
test01()
{
  std::deque<int, Alloc<int>> d;
  auto d2 = std::move(d);
}
