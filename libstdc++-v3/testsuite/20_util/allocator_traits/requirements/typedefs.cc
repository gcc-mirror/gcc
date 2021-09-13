// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// 
// NB: This file is for testing memory with NO OTHER INCLUDES.

#include <memory>

template<typename T>
void test01()
{
  // Check for required typedefs
  typedef std::allocator_traits<T>                  test_type;
  typedef typename test_type::allocator_type        allocator_type;
  typedef typename test_type::value_type            value_type;
  typedef typename test_type::pointer               pointer;
  typedef typename test_type::const_pointer         const_pointer;
  typedef typename test_type::void_pointer          void_pointer;
  typedef typename test_type::const_void_pointer    const_void_pointer;
  typedef typename test_type::difference_type       difference_type;
  typedef typename test_type::size_type             size_type;
  typedef typename test_type::propagate_on_container_copy_assignment
    propagate_on_container_copy_assignment;
  typedef typename test_type::propagate_on_container_move_assignment
    propagate_on_container_move_assignment;
  typedef typename test_type::propagate_on_container_swap
    propagate_on_container_swap;
}

struct S { };

int main()
{
  test01<std::allocator<int>>();
  test01<std::allocator<S>>();
}
