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

// { dg-do compile { target c++11 } }

#include <forward_list>

template <typename T>
struct Alloc
{
  using value_type = T;

  T* allocate(unsigned n);
  void deallocate(T* p, unsigned n);

  Alloc(int) { }

  template<typename U>
    Alloc(const Alloc<U>&) { }
};

template<typename T, typename U>
  bool operator==(Alloc<T>, Alloc<U>) { return true; }
template<typename T, typename U>
  bool operator!=(Alloc<T>, Alloc<U>) { return false; }

constexpr bool b
  = std::is_default_constructible<std::forward_list<bool, Alloc<bool>>>::value;
