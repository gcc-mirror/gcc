// Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

// { dg-do compile { target c++20 } }

#include <concepts>

static_assert( std::swappable_with<int&, int&> );
static_assert( ! std::swappable_with<int, int> );
static_assert( ! std::swappable_with<int*, int*> );
static_assert( ! std::swappable_with<int&, const int&> );
static_assert( ! std::swappable_with<const int&, const int&> );
static_assert( ! std::swappable_with<int&, long&> );

namespace N1
{
  struct Immovable
  {
    Immovable() = default;
    Immovable(Immovable&&) = delete;
  };
}
static_assert( ! std::swappable_with<N1::Immovable&, N1::Immovable&> );

namespace N2
{
  struct Swappable
  {
    Swappable() = default;
    Swappable(Swappable&&) = delete;
    friend void swap(Swappable&, Swappable&) { }
  };
}
static_assert( std::swappable_with<N2::Swappable&, N2::Swappable&> );

namespace N3
{
  struct A { };
  struct Proxy {
    Proxy(A&) { }
    friend void swap(Proxy, Proxy) { }
  };
}

static_assert( std::swappable_with<N3::A&, N3::A&> );
static_assert( std::swappable_with<N3::A&, N3::Proxy> );
static_assert( std::swappable_with<N3::Proxy, N3::A&> );
static_assert( std::swappable_with<N3::Proxy, N3::Proxy> );

struct C { C(int&) { } };
void swap(int&, C&) { } // not symmetrical
static_assert( ! std::swappable_with<int, C> );
static_assert( ! std::swappable_with<C, int> );

struct D { D(int&) { } };
void swap(int&, D&) { }
void swap(D&&, int&) { } // only accepts rvalues
static_assert( ! std::swappable_with<int&, D> );
static_assert( ! std::swappable_with<D, int> );

struct E { E(int&) { } };
void swap(int, E&&) { } // only accepts rvalues
void swap(E&, int) { }
static_assert( ! std::swappable_with<int, E> );
static_assert( ! std::swappable_with<E, int> );
