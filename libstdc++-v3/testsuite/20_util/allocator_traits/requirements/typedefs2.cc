// { dg-options "-std=gnu++11" }
//
// Copyright (C) 2013 Free Software Foundation, Inc.
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

#include <memory>
#include <testsuite_allocator.h>

// { dg-do compile }

template<typename T>
struct ptr
{
  ptr() = default;
  template<typename U>
    ptr(ptr<U> const& p) { }
};

// This doesn't meet the allocator requirements, it's only to check
// that allocator_traits finds the right nested types.
template<typename T>
struct alloc
{
  typedef T value_type;

  typedef ptr<T> pointer;
  typedef ptr<const T> const_pointer;
  typedef ptr<void> void_pointer;
  typedef ptr<const void> const_void_pointer;
  typedef int difference_type;
  typedef int size_type;

  typedef std::false_type propagate_on_container_copy_assignment;
  typedef std::false_type propagate_on_container_move_assignment;
  typedef std::false_type propagate_on_container_swap;
};

typedef alloc<int> alloc_type;
typedef std::allocator_traits<alloc_type> traits;

using std::is_same;

static_assert( is_same<traits::pointer, alloc_type::pointer>::value,
               "pointer" );

static_assert( is_same<traits::const_pointer,
                       alloc_type::const_pointer>::value,
               "const_pointer" );

static_assert( is_same<traits::void_pointer, alloc_type::void_pointer>::value,
               "void_pointer" );

static_assert( is_same<traits::const_void_pointer,
                       alloc_type::const_void_pointer>::value,
               "const_void_pointer");

static_assert( is_same<traits::difference_type,
                       alloc_type::difference_type>::value,
               "difference_type" );

static_assert( is_same<traits::size_type, alloc_type::size_type>::value,
               "size_type" );

static_assert( is_same<traits::size_type, alloc_type::size_type>::value,
               "size_type" );

static_assert( is_same<traits::propagate_on_container_copy_assignment,
                       alloc_type::propagate_on_container_copy_assignment
                      >::value,
               "propagate_on_container_copy_assignment" );

static_assert( is_same<traits::propagate_on_container_move_assignment,
                       alloc_type::propagate_on_container_move_assignment
                      >::value,
               "propagate_on_container_move_assignment" );

static_assert( is_same<traits::propagate_on_container_swap,
                       alloc_type::propagate_on_container_swap>::value,
               "propagate_on_container_swap" );
