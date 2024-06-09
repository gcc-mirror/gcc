// Copyright (C) 2016-2024 Free Software Foundation, Inc.
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

#include <scoped_allocator>
#include <testsuite_allocator.h>

template<typename T>
struct alloc
{
  using value_type = T;
  alloc() = default;
  template<typename U>
    alloc(alloc<U>) { }
  T* allocate(std::size_t);
  void deallocate(T*, std::size_t);
};

template<typename T, typename U>
  bool operator==(alloc<T>, alloc<U>) { return true; }

template<typename T, typename U>
  bool operator!=(alloc<T>, alloc<U>) { return false; }

using scoped = std::scoped_allocator_adaptor<alloc<int>>;
using other_alloc = __gnu_test::SimpleAllocator<int>;
using other_scoped = std::scoped_allocator_adaptor<other_alloc>;

using std::is_constructible;

static_assert( is_constructible<scoped, const scoped&>::value,
    "is_constructible<scoped, const scoped&>");
static_assert( is_constructible<scoped, scoped>::value,
    "is_constructible<scoped, scoped>");
static_assert( is_constructible<scoped, const alloc<int>&>::value,
    "is_constructible<scoped, const outer_allocator_type&>");
static_assert( is_constructible<scoped, alloc<int>>::value,
    "is_constructible<scoped, outer_allocator_type>");
static_assert( is_constructible<scoped, const alloc<long>&>::value,
    "is_constructible<scoped, const outer_allocator_type::rebind<U>::type&>");
static_assert( is_constructible<scoped, alloc<long>>::value,
    "is_constructible<scoped, outer_allocator_type::rebind<U>::type>");

static_assert( !is_constructible<scoped, const other_alloc&>::value,
    "!is_constructible<scoped, const other_alloc&>");
static_assert( !is_constructible<scoped, other_alloc>::value,
    "!is_constructible<scoped, other_alloc>");
static_assert( !is_constructible<scoped, const other_scoped&>::value,
    "!is_constructible<scoped, const other_scoped&>");
static_assert( !is_constructible<scoped, other_scoped>::value,
    "!is_constructible<scoped, other_scoped>");
