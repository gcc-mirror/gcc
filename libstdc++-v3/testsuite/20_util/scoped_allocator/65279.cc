// Copyright (C) 2015-2018 Free Software Foundation, Inc.
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

#include <memory>
#include <type_traits>
#include <scoped_allocator>

template<typename T>
  struct Allocator : std::allocator<T>
  {
    template<typename U>
      struct rebind { using other = Allocator<U>; };

    using propagate_on_container_copy_assignment = std::true_type;
    using propagate_on_container_move_assignment = std::true_type;
  };

template<typename... T>
  using alloc = std::scoped_allocator_adaptor<Allocator<T>...>;

void
test01()
{
  // Test partial specialization for sizeof...(InnerAlloc) == 0
  alloc<int> a;
  a = a;
  a = std::move(a);
}

void
test02()
{
  // Test partial specialization for sizeof...(InnerAlloc) >= 1
  alloc<int, char> a;
  a = a;
  a = std::move(a);
}
