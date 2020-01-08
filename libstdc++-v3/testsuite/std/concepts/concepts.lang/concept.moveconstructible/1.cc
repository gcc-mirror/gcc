// Copyright (C) 2019-2020 Free Software Foundation, Inc.
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

#include <concepts>

static_assert( !std::move_constructible<void> );
static_assert( std::move_constructible<void*> );
static_assert( std::move_constructible<const void*> );
static_assert( std::move_constructible<char> );
static_assert( std::move_constructible<const float> );
static_assert( std::move_constructible<int*> );
static_assert( std::move_constructible<int&> );
static_assert( std::move_constructible<int&&> );
static_assert( std::move_constructible<const int&> );
static_assert( !std::move_constructible<int[]> );
static_assert( !std::move_constructible<int[2]> );
static_assert( !std::move_constructible<int()> );
static_assert( std::move_constructible<int(*)()> );
static_assert( std::move_constructible<int(&)()> );
static_assert( std::move_constructible<int(&)() noexcept> );

enum E { };
static_assert( std::move_constructible<E> );
enum class CE { };
static_assert( std::move_constructible<CE> );
struct A { };
static_assert( std::move_constructible<A> );
union B { };
static_assert( std::move_constructible<B> );

struct C
{
  C(void* = nullptr) { }
  ~C() noexcept(false) { }
};
static_assert( !std::move_constructible<C> );

class D
{
public:
  D() { }
  D(int) { }
private:
  ~D() { }
};
static_assert( !std::move_constructible<D> );
