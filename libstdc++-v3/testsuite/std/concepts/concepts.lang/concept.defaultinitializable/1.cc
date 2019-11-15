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

#include <concepts>

static_assert( !std::default_initializable<void> );
static_assert( std::default_initializable<void*> );
static_assert( std::default_initializable<const void*> );
static_assert( std::default_initializable<char> );
static_assert( std::default_initializable<float> );
static_assert( !std::default_initializable<const int> );
static_assert( std::default_initializable<int*> );
static_assert( !std::default_initializable<int&> );
static_assert( !std::default_initializable<int&&> );
static_assert( !std::default_initializable<const int&> );
static_assert( !std::default_initializable<int[]> );
static_assert( std::default_initializable<int[2]> );
static_assert( !std::default_initializable<int()> );
static_assert( std::default_initializable<int(*)()> );
static_assert( !std::default_initializable<int(&)()> );

enum E { };
static_assert( std::default_initializable<E> );
enum class CE { };
static_assert( std::default_initializable<CE> );
struct A { };
static_assert( std::default_initializable<A> );
union B { };
static_assert( std::constructible_from<B> );

struct C
{
  C(void* = nullptr) { }
  ~C() noexcept(false) { }
};
static_assert( !std::default_initializable<C> );

class D
{
public:
  D() { }
  D(int) { }
private:
  ~D() { }
};
static_assert( !std::default_initializable<D> );
