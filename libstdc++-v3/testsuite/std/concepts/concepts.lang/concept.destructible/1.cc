// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

static_assert( !std::destructible<void> );
static_assert( std::destructible<char> );
static_assert( std::destructible<float> );
static_assert( std::destructible<int*> );
static_assert( std::destructible<int&> );
static_assert( std::destructible<int&&> );
static_assert( std::destructible<const int&> );
static_assert( !std::destructible<int[]> );
static_assert( std::destructible<int[2]> );
static_assert( std::destructible<int[2][3]> );
static_assert( !std::destructible<int()> );
static_assert( std::destructible<int(*)()> );
static_assert( std::destructible<int(&)()> );

enum E { };
static_assert( std::destructible<E> );
enum class CE { };
static_assert( std::destructible<CE> );
struct A { };
static_assert( std::destructible<A> );
union B { };
static_assert( std::destructible<B> );

struct C
{
  ~C() noexcept(false) { }
};
static_assert( !std::destructible<C> );
static_assert( std::destructible<C&> );
static_assert( !std::destructible<C[1]> );
class D
{
public:
  D() { }
private:
  ~D() { }
};
static_assert( !std::destructible<D> );
static_assert( std::destructible<D&> );
static_assert( !std::destructible<D[1]> );
