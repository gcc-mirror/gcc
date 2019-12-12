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

static_assert( std::copyable<int> );
static_assert( std::copyable<int*> );
static_assert( ! std::copyable<int&> );
static_assert( ! std::copyable<void> );
static_assert( ! std::copyable<void()> );
static_assert( ! std::copyable<void() noexcept> );
static_assert( ! std::copyable<void() const> );

struct Trivial { };
static_assert( std::copyable<Trivial> );

struct NotTrivial
{
  NotTrivial(int) { }
  NotTrivial(const NotTrivial&) { }
  NotTrivial& operator=(const NotTrivial&) { return *this; }
  ~NotTrivial() { }
};
static_assert( std::copyable<NotTrivial> );

namespace N1
{
  struct Immovable
  {
    Immovable() = default;
    Immovable(Immovable&&) = delete;
  };
}
static_assert( ! std::copyable<N1::Immovable> );

struct Movable
{
  Movable() = default;
  Movable(Movable&&) = default;
  Movable& operator=(Movable&&) = default;
};
static_assert( ! std::copyable<Movable> );

struct MovableAndCopyAssignable
{
  MovableAndCopyAssignable() = default;
  MovableAndCopyAssignable(MovableAndCopyAssignable&&) = default;
  MovableAndCopyAssignable& operator=(MovableAndCopyAssignable&&) = default;
  MovableAndCopyAssignable& operator=(const MovableAndCopyAssignable&) = default;
};
static_assert( ! std::copyable<MovableAndCopyAssignable> );

struct MovableAndCopyConstructible
{
  MovableAndCopyConstructible() = default;
  MovableAndCopyConstructible(MovableAndCopyConstructible&&) = default;
  MovableAndCopyConstructible(const MovableAndCopyConstructible&) = default;
  MovableAndCopyConstructible& operator=(MovableAndCopyConstructible&&) = default;
};
static_assert( ! std::copyable<MovableAndCopyConstructible> );

namespace N2
{
  struct Swappable
  {
    Swappable() = default;
    Swappable(Swappable&&) = default;
    friend void swap(Swappable&, Swappable&) { }
  };
}
static_assert( ! std::copyable<N2::Swappable> );

struct NotAssignable
{
  NotAssignable() = default;
  NotAssignable(NotAssignable&&) = default;
  NotAssignable& operator=(NotAssignable&&) = default;
  NotAssignable(const NotAssignable&) = default;
  NotAssignable& operator=(const NotAssignable&) = delete;
  friend void swap(NotAssignable&, NotAssignable&) { }
};
static_assert( ! std::copyable<NotAssignable> );

struct NotSwappable
{
  NotSwappable() = default;
  NotSwappable(const NotSwappable&) = default;
  NotSwappable& operator=(const NotSwappable&) = default;
};
void swap(NotSwappable&, NotSwappable&) = delete;
static_assert( std::copyable<NotSwappable> ); // ranges::swap still works!
