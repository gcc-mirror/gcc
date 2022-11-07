// { dg-do compile { target c++11 } }
// FIXME [!HOSTED]: avoidable std::allocator use
// { dg-require-effective-target hosted }

// Copyright (C) 2011-2022 Free Software Foundation, Inc.
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

#include <scoped_allocator>
#include <memory>

typedef short test_type;

template<typename T>
  struct minimal_allocator
  {
    typedef T value_type;
    minimal_allocator();
    template <typename U>
      minimal_allocator(const minimal_allocator<U>&);
    T* allocate(unsigned long);
    void deallocate(T*, unsigned long);
  };

namespace std
{
  template struct scoped_allocator_adaptor<std::allocator<test_type>>;

  template struct scoped_allocator_adaptor<minimal_allocator<test_type>>;

  template struct scoped_allocator_adaptor<std::allocator<test_type>,
                                           minimal_allocator<test_type>>;
}
