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

static_assert( !std::assignable_from<void, void> );
static_assert( !std::assignable_from<void*, int*> );
static_assert( std::assignable_from<void*&, int*> );
static_assert( !std::assignable_from<void*&, const int*> );
static_assert( std::assignable_from<const void*&, const int*> );
static_assert( !std::assignable_from<char, char> );
static_assert( std::assignable_from<char&, char> );
static_assert( !std::assignable_from<float, float> );
static_assert( std::assignable_from<float&, double> );
static_assert( std::assignable_from<int*&, int*> );
static_assert( std::assignable_from<int&, int&> );
static_assert( std::assignable_from<int&, int&&> );
static_assert( !std::assignable_from<int&&, int> );
static_assert( !std::assignable_from<const int&, int> );
static_assert( !std::assignable_from<const int&, int&> );
static_assert( !std::assignable_from<const int&, const int> );
static_assert( !std::assignable_from<const int&, const int&> );
static_assert( !std::assignable_from<int(&)[], int(&)[]> );
static_assert( !std::assignable_from<int(&)[], int> );
static_assert( !std::assignable_from<int(&)[2], int(&)[2]> );
static_assert( !std::assignable_from<int(), int()> );
static_assert( !std::assignable_from<int(*)(), int(*)()> );
static_assert( std::assignable_from<int(*&)(), int(*)()> );
static_assert( std::assignable_from<int(*&)(), std::nullptr_t> );
static_assert( std::assignable_from<int(*&)(), int(*)() noexcept> );
static_assert( std::assignable_from<int(*&)(), int(&)() noexcept> );
static_assert( !std::assignable_from<int(&)(), std::nullptr_t> );
static_assert( !std::assignable_from<int(&)(), int(&)() noexcept> );

enum E { };
static_assert( !std::assignable_from<E, E> );
static_assert( std::assignable_from<E&, E> );
static_assert( std::assignable_from<E&, E&> );
static_assert( std::assignable_from<E&, const E&> );
static_assert( !std::assignable_from<const E&, const E&> );
enum class CE { };
static_assert( !std::assignable_from<CE, CE> );
static_assert( std::assignable_from<CE&, CE&> );
static_assert( std::assignable_from<CE&, const CE&> );
static_assert( !std::assignable_from<const CE&, const CE&> );
struct A { };
static_assert( !std::assignable_from<A, A> );
static_assert( std::assignable_from<A&, A> );
static_assert( !std::assignable_from<A, A&> );
static_assert( std::assignable_from<A&, const A&> );
union B { };
static_assert( !std::assignable_from<B, B> );
static_assert( std::assignable_from<B&, B> );
static_assert( !std::assignable_from<B, B&> );
static_assert( std::assignable_from<B&, const B&> );

struct C
{
  C(int) { }
  C& operator=(const C&) { return *this; }
  C& operator=(int) { return *this; }
  void operator=(void*) { }
};
static_assert( std::assignable_from<C&, C> );
static_assert( std::assignable_from<C&, const C&> );
static_assert( std::assignable_from<C&, int> );
static_assert( !std::assignable_from<C&, void*> );
static_assert( !std::assignable_from<C&, std::nullptr_t> );
