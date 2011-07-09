// { dg-do compile }
// { dg-options "-std=gnu++0x" }

// Copyright (C) 2011 Free Software Foundation, Inc.
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

// test that propagate_on_container_xxx is true iff it is true for
// any of the outer or inner allocators

#include <scoped_allocator>

using std::scoped_allocator_adaptor;

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

template<typename T, bool copy, bool move, bool swap>
  struct test_allocator : minimal_allocator<T>
  {
    struct propagate_on_container_copy_assignment
    { static const bool value = copy; };

    struct propagate_on_container_move_assignment
    { static const bool value = move; };

    struct propagate_on_container_swap
    { static const bool value = swap; };
  };

template<typename A>
  constexpr bool prop_on_copy()
  {
    typedef typename A::propagate_on_container_copy_assignment type;
    return type::value;
  }

template<typename A>
  constexpr bool prop_on_move()
  {
    typedef typename A::propagate_on_container_move_assignment type;
    return type::value;
  }

template<typename A>
  constexpr bool prop_on_swap()
  {
    typedef typename A::propagate_on_container_swap type;
    return type::value;
  }

template<typename A, bool C, bool M, bool S>
  constexpr bool test1()
  {
    static_assert( prop_on_copy<A>() == C, "copy" );
    static_assert( prop_on_move<A>() == M, "move" );
    static_assert( prop_on_swap<A>() == S, "swap" );
    return true;
  }

template<bool C, bool M, bool S>
  constexpr bool test2()
  {
    typedef minimal_allocator<test_type>       base_alloc;
    typedef test_allocator<test_type, C, M, S> test_alloc;
    typedef scoped_allocator_adaptor<base_alloc, test_alloc> scoped1;
    typedef scoped_allocator_adaptor<test_alloc, base_alloc> scoped2;
    typedef scoped_allocator_adaptor<test_alloc, test_alloc> scoped3;
    return test1<scoped1, C, M, S>()
        && test1<scoped2, C, M, S>()
        && test1<scoped3, C, M, S>();
  }

static_assert( test2<false, false, false>(), "never propagate" );
static_assert( test2<true, false, false>(), "propagate on copy" );
static_assert( test2<false, true, false>(), "propagate on move" );
static_assert( test2<false, false, true>(), "propagate on swap" );
static_assert( test2<true, true, false>(), "propagate on copy & move" );
static_assert( test2<true, false, true>(), "propagate on copy & swap" );
static_assert( test2<false, true, true>(), "propagate on move & swap" );
static_assert( test2<true, true, true>(), "always propagate" );

