// { dg-options "-std=gnu++17" }
// { dg-do compile }

// Copyright (C) 2016-2018 Free Software Foundation, Inc.
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

#include "../../../../../include/std/pmroptional"

// Swappable.
struct A {
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  A(std::allocator_arg_t,allocator_type) noexcept(true)
  : A(){}

  A(std::allocator_arg_t,allocator_type, A const& other) noexcept(true)
    : A(other){}

  A(std::allocator_arg_t,allocator_type, A &&other) noexcept(true)
    : A(std::move(other)){}

  A() noexcept(true) = default;
  A(A const&) noexcept(true)  = default;
  A(A &&) noexcept(true) = default;


};
void swap(A&, A&) noexcept(true);

static_assert( std::is_nothrow_swappable_v<A> );
static_assert( std::is_nothrow_swappable_v<std::pmr::optional<A>> );

// Swappable, but might throw.
struct B {
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  B(std::allocator_arg_t,allocator_type)
  : B(){}

  B(std::allocator_arg_t,allocator_type, B const& other)
    : B(other){}

  B(std::allocator_arg_t,allocator_type, B &&other)
    : B(std::move(other)){}

  B() = default;
  B(B const&) = default;
  B(B &&) = default;
};
void swap(B&, B&) noexcept(false);

static_assert( std::is_swappable_v<std::pmr::optional<B>> );
static_assert( !std::is_nothrow_swappable_v<std::pmr::optional<B>> );

// Not swappable, and optional<C> not swappable via the generic std::swap.
struct C {
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  C(std::allocator_arg_t,allocator_type)
  : C(){}

  C(std::allocator_arg_t,allocator_type, C const& other)
    : C(other){}

  C(std::allocator_arg_t,allocator_type, C &&other)
    : C(std::move(other)){}

  C() = default;
  C(C const&) = default;
  C(C &&) = default;
};
void swap(C&, C&) = delete;

static_assert( !std::is_swappable_v<std::pmr::optional<C>> );

// Not swappable, and optional<D> not swappable via the generic std::swap.
struct D {
  typedef std::pmr::polymorphic_allocator<void> allocator_type;

  D(std::allocator_arg_t,allocator_type)
  : D(){}

  D(std::allocator_arg_t,allocator_type, D const& other)
    : D(other){}


  D() = default;
  D(D const&) = default;

  D(D&&) = delete;
};

static_assert( !std::is_swappable_v<std::pmr::optional<D>> );
