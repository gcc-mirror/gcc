// Copyright (C) 2019 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do compile { target c++2a } }

#include <memory>

template<typename T> struct Alloc : std::allocator<T> { };

using T = std::allocator_traits<Alloc<int>>;
// Prior to C++20 this finds std::allocator<int>::rebind and so fails:
static_assert( std::is_same_v<T::rebind_alloc<long>, Alloc<long>> );

using V = std::allocator_traits<Alloc<void>>;
// Prior to C++20 this finds std::allocator<void>::rebind and so fails:
static_assert( std::is_same_v<V::rebind_alloc<long>, Alloc<long>> );
