// { dg-do compile { target c++17 }  }

// Copyright (C) 2020-2024 Free Software Foundation, Inc.
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

#include <optional>

int i;

struct Cont
{
  Cont() noexcept;
  Cont(Cont&&) noexcept;
  Cont(const Cont&);
  Cont(int);
  Cont(std::initializer_list<int>, int) noexcept;
  Cont(std::initializer_list<int>, const char*);
};
const Cont c{};

template<typename T, typename = void>
  struct can_make_optional1
  : std::false_type
  { };

template<typename T>
  struct can_make_optional1<T,
      std::void_t<decltype(std::make_optional(std::declval<T>()))>>
  : std::true_type
  { };

static_assert( can_make_optional1<int>::value );
static_assert( noexcept(std::make_optional(1)) );
static_assert( can_make_optional1<int&>::value );
static_assert( noexcept(std::make_optional(i)) );
static_assert( ! can_make_optional1<void>::value );
static_assert( can_make_optional1<Cont>::value );
static_assert( noexcept(std::make_optional(Cont{})) );
static_assert( can_make_optional1<Cont>::value );
static_assert( ! noexcept(std::make_optional(c)) );

template<typename T, typename Arg, typename = void>
  struct can_make_optional2
  : std::false_type
  { };

template<typename T, typename Arg>
  struct can_make_optional2<T, Arg,
      std::void_t<decltype(std::make_optional<T>(std::declval<Arg>()))>>
  : std::true_type
  { };

static_assert( can_make_optional2<int, int>::value );
static_assert( noexcept(std::make_optional<int>(1)) );
static_assert( can_make_optional2<int, int&>::value );
static_assert( noexcept(std::make_optional(i)) );
static_assert( ! can_make_optional2<void, void>::value );
static_assert( can_make_optional2<Cont, Cont>::value );
static_assert( noexcept(std::make_optional<Cont>({})) );
static_assert( can_make_optional2<Cont, const Cont&>::value );
static_assert( ! noexcept(std::make_optional(c)) );
static_assert( can_make_optional2<Cont, int>::value );
static_assert( ! noexcept(std::make_optional<Cont>(1)) );

template<typename T, typename Arg, typename = void>
  struct can_make_optional3
  : std::false_type
  { };

template<typename T, typename Arg>
  struct can_make_optional3<T, Arg,
      std::void_t<decltype(std::make_optional<T>({1,2}, std::declval<Arg>()))>>
  : std::true_type
  { };

static_assert( can_make_optional3<Cont, int>::value );
static_assert( noexcept(std::make_optional<Cont>({1,2}, 1)) );
static_assert( can_make_optional3<Cont, char*>::value );
static_assert( ! noexcept(std::make_optional<Cont>({1,2}, "")) );
static_assert( !can_make_optional3<Cont, int*>::value );
