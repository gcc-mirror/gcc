// Copyright (C) 2019-2021 Free Software Foundation, Inc.
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

static_assert( std::movable<int> );
static_assert( std::movable<int*> );
static_assert( ! std::movable<int&> );
static_assert( ! std::movable<void> );
static_assert( ! std::movable<void()> );
static_assert( ! std::movable<void() noexcept> );
static_assert( ! std::movable<void() const> );

struct Trivial { };
static_assert( std::movable<Trivial> );

struct NotTrivial
{
  NotTrivial(int) { }
  NotTrivial(NotTrivial&&) { }
  NotTrivial& operator=(NotTrivial&&) { return *this; }
  ~NotTrivial() { }
};
static_assert( std::movable<NotTrivial> );

namespace N1
{
  struct Immovable
  {
    Immovable() = default;
    Immovable(Immovable&&) = delete;
  };
}
static_assert( ! std::movable<N1::Immovable> );

namespace N2
{
  struct Swappable
  {
    Swappable() = default;
    Swappable(Swappable&&) = delete;
    Swappable& operator=(Swappable&&) = default;
    friend void swap(Swappable&, Swappable&) { }
  };
}
static_assert( ! std::movable<N2::Swappable> );

struct NotAssignable
{
  NotAssignable() = default;
  NotAssignable(NotAssignable&&) = default;
  NotAssignable& operator=(NotAssignable&&) = delete;
  friend void swap(NotAssignable&, NotAssignable&) { }
};
static_assert( ! std::movable<NotAssignable> );

struct NotSwappable
{
  NotSwappable() = default;
  NotSwappable(NotSwappable&&) = default;
  NotSwappable& operator=(NotSwappable&&) = default;
};
void swap(NotSwappable&, NotSwappable&) = delete;
static_assert( std::movable<NotSwappable> ); // ranges::swap still works!
