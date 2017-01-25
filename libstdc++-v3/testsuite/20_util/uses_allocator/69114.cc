// Copyright (C) 2016-2017 Free Software Foundation, Inc.
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

// PR libstdc++/69114

#include <tuple>

template<typename T>
struct Alloc
{
  using value_type = T;

  Alloc() = default;

  template<typename U>
    Alloc(const Alloc<U>&) { }

  T* allocate(std::size_t);
  void deallocate(T*, std::size_t);

  bool operator==(const Alloc&) const { return true; }
  bool operator!=(const Alloc&) const { return false; }

  void operator&() = delete;
};

void
test01()
{
  Alloc<int> a;
  std::tuple<int> t(std::allocator_arg, a);
}
