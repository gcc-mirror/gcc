// { dg-do compile { target c++11 } }

// Copyright (C) 2011-2017 Free Software Foundation, Inc.
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

// NB: This file is for testing memory with NO OTHER INCLUDES.

#include <memory>

typedef short test_type;

template<typename T>
  struct minimal_allocator
  {
    typedef T value_type;
    minimal_allocator();
    template <typename U>
      minimal_allocator(const minimal_allocator<U>&);
    T* allocate(std::size_t);
    void deallocate(T*, std::size_t);
  };

namespace std
{
  template struct allocator_traits<std::allocator<test_type>>;
  template struct allocator_traits<minimal_allocator<test_type>>;
}
