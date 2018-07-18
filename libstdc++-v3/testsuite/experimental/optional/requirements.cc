// { dg-do run { target c++14 } }

// Copyright (C) 2013-2018 Free Software Foundation, Inc.
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

#include <experimental/optional>
#include <testsuite_hooks.h>

#include <tuple>

using std::experimental::bad_optional_access;
static_assert( std::is_default_constructible<bad_optional_access>::value, "" );

struct trivially_destructible
{
  trivially_destructible() = delete;
  trivially_destructible(trivially_destructible const&) = delete;
  trivially_destructible& operator=(trivially_destructible const&) = delete;
  trivially_destructible(trivially_destructible&&) = delete;
  trivially_destructible& operator=(trivially_destructible&&) = delete;
  ~trivially_destructible() noexcept = default;
};

static_assert( std::is_trivially_destructible<trivially_destructible>(), "" );

struct no_default_constructor
{
  no_default_constructor() = delete;
};

struct no_copy_constructor
{
  no_copy_constructor() = default;
  no_copy_constructor(no_copy_constructor const&) = delete;
  no_copy_constructor& operator=(no_copy_constructor const&) = default;
  no_copy_constructor(no_copy_constructor&&) = default;
  no_copy_constructor& operator=(no_copy_constructor&&) = default;
};

struct no_copy_assignment
{
  no_copy_assignment() = default;
  no_copy_assignment(no_copy_assignment const&) = default;
  no_copy_assignment(no_copy_assignment&&) = default;
  no_copy_assignment& operator=(no_copy_assignment&&) = default;
};

struct no_move_constructor
{
  no_move_constructor() = default;
  no_move_constructor(no_move_constructor const&) = default;
  no_move_constructor& operator=(no_move_constructor const&) = default;
  no_move_constructor(no_move_constructor&&) = delete;
  no_move_constructor& operator=(no_move_constructor&&) = default;
};

struct no_move_assignment
{
  no_move_assignment() = default;
  no_move_assignment(no_move_assignment const&) = default;
  no_move_assignment& operator=(no_move_assignment const&) = default;
  no_move_assignment(no_move_assignment&&) = default;
  no_move_assignment& operator=(no_move_assignment&&) = delete;
};

struct no_copy : no_copy_constructor, no_copy_assignment { };
struct no_move : no_move_constructor, no_move_assignment { };

// Laxest possible model of a value type for optional
struct only_destructible
{
  only_destructible(only_destructible&&) = delete;
};

int main()
{
  {
    static_assert( std::is_trivially_destructible<std::experimental::optional<trivially_destructible>>(), "" );
  }

  {
    using T = no_default_constructor;
    using O = std::experimental::optional<T>;
    static_assert( std::is_same<O::value_type, T>(), "" );
    static_assert( std::is_default_constructible<O>(), "" );
    { O o; }
    static_assert( std::is_copy_constructible<O>(), "" );
    { O o; auto copy = o; }
    static_assert( std::is_copy_assignable<O>(), "" );
    { O o, p; p = o; }
    static_assert( std::is_move_constructible<O>(), "" );
    { O o; auto moved_to = std::move(o); }
    static_assert( std::is_move_assignable<O>(), "" );
    { O o, p; p = std::move(o); }
  }

  {
    using T = no_copy_constructor;
    using O = std::experimental::optional<T>;
    static_assert( std::is_same<O::value_type, T>(), "" );
    static_assert( std::is_default_constructible<O>(), "" );
    { O o; }
    static_assert( !std::is_copy_constructible<O>(), "" );
    static_assert( !std::is_copy_assignable<O>(), "" );
    static_assert( std::is_move_constructible<O>(), "" );
    { O o; auto moved_to = std::move(o); }
    static_assert( std::is_move_assignable<O>(), "" );
    { O o, p; p = std::move(o); }
  }

  {
    using T = no_copy_assignment;
    using O = std::experimental::optional<T>;
    static_assert( std::is_default_constructible<O>(), "" );
    { O o; }
    static_assert( std::is_copy_constructible<O>(), "" );
    { O o; auto copy = o; }
    static_assert( !std::is_copy_assignable<O>(), "" );
    static_assert( std::is_move_constructible<O>(), "" );
    { O o; auto moved_to = std::move(o); }
    static_assert( std::is_move_assignable<O>(), "" );
    { O o, p; p = std::move(o); }
  }

  {
    using T = no_copy;
    using O = std::experimental::optional<T>;
    static_assert( std::is_same<O::value_type, T>(), "" );
    static_assert( std::is_default_constructible<O>(), "" );
    { O o; }
    static_assert( !std::is_copy_constructible<O>(), "" );
    static_assert( !std::is_copy_assignable<O>(), "" );
    static_assert( std::is_move_constructible<O>(), "" );
    { O o; auto moved_to = std::move(o); }
    static_assert( std::is_move_assignable<O>(), "" );
    { O o, p; p = std::move(o); }
  }

  {
    using T = no_move_constructor;
    using O = std::experimental::optional<T>;
    static_assert( std::is_same<O::value_type, T>(), "" );
    static_assert( std::is_default_constructible<O>(), "" );
    { O o; }
    static_assert( std::is_copy_constructible<O>(), "" );
    { O o; auto copy = o; }
    static_assert( std::is_copy_assignable<O>(), "" );
    /*
     * T should be move constructible due to [12.8/11], which is a new rule in C++1y
     * not yet implemented by GCC. Because there is already a special exception in C++11
     * for the generation of the special members that GCC implements (at least some of the
     * time), this does not affect the std::experimental::optional implementation however. So the assertion
     * for T should be changed (or removed altogether) when the time comes, but the rest
     * should however remain correct and unchanged.
     */
    static_assert( !std::is_move_constructible<T>(), "" );
    static_assert( std::is_move_constructible<O>(), "" );
    { O o; auto moved_to = std::move(o); }
    static_assert( std::is_move_assignable<O>(), "" );
    { O o, p; p = std::move(o); }
  }

  {
    using T = no_move_assignment;
    using O = std::experimental::optional<T>;
    static_assert( std::is_same<O::value_type, T>(), "" );
    static_assert( std::is_default_constructible<O>(), "" );
    { O o; }
    static_assert( std::is_copy_constructible<O>(), "" );
    { O o; auto copy = o; }
    static_assert( std::is_copy_assignable<O>(), "" );
    { O o, p; p = o; }
    static_assert( std::is_move_constructible<O>(), "" );
    { O o; auto moved_to = std::move(o); }
    /*
     * Paragraph 23 of same leads to a similar situation but with respect to move
     * assignment.
     */
    static_assert( !std::is_move_assignable<T>(), "" );
    static_assert( std::is_move_assignable<O>(), "" );
    { O o, p; p = std::move(o); }
  }

  {
    using T = no_move;
    using O = std::experimental::optional<T>;
    static_assert( std::is_same<O::value_type, T>(), "" );
    static_assert( std::is_default_constructible<O>(), "" );
    { O o; }
    static_assert( std::is_copy_constructible<O>(), "" );
    { O o; auto copy = o; }
    static_assert( std::is_copy_assignable<O>(), "" );
    { O o, p; p = o; }
    static_assert( std::is_move_constructible<O>(), "" );
    { O o; auto moved_to = std::move(o); }
    static_assert( std::is_move_assignable<O>(), "" );
    { O o, p; p = std::move(o); }
  }

  {
    using T = only_destructible;
    using O = std::experimental::optional<T>;
    static_assert( std::is_same<O::value_type, T>(), "" );
    static_assert( std::is_default_constructible<O>(), "" );
    { O o; }
    static_assert( !std::is_copy_constructible<O>(), "" );
    static_assert( !std::is_copy_assignable<O>(), "" );
    static_assert( !std::is_move_constructible<O>(), "" );
    static_assert( !std::is_move_assignable<O>(), "" );
  }

  {
    /*
     * Should not complain about 'invalid' specializations as long as
     * they're not instantiated.
     */
    using A = std::experimental::optional<int&>;
    using B = std::experimental::optional<int&&>;
    using C1 = std::experimental::optional<std::experimental::in_place_t>;
    using C2 = std::experimental::optional<std::experimental::in_place_t const>;
    using C3 = std::experimental::optional<std::experimental::in_place_t volatile>;
    using C4 = std::experimental::optional<std::experimental::in_place_t const volatile>;
    using D1 = std::experimental::optional<std::experimental::nullopt_t>;
    using D2 = std::experimental::optional<std::experimental::nullopt_t const>;
    using D3 = std::experimental::optional<std::experimental::nullopt_t volatile>;
    using D4 = std::experimental::optional<std::experimental::nullopt_t const volatile>;

    using X = std::tuple<A, B, C1, C2, C3, C4, D1, D2, D3, D4>;
  }

  {
    std::experimental::optional<const int> o { 42 };
    static_assert( std::is_same<decltype(o)::value_type, const int>(), "" );
    VERIFY( o );
    VERIFY( *o == 42 );
  }

  {
    constexpr std::experimental::optional<const int> o { 33 };
    static_assert( std::is_same<decltype(o)::value_type, const int>(), "" );
    static_assert( o, "" );
    static_assert( *o == 33, "" );
  }
}
