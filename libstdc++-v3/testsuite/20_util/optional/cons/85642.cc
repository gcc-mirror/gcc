// Copyright (C) 2018 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++17" }
// { dg-do compile { target c++17 } }

#include <optional>

struct NonTrivialDtor {
  ~NonTrivialDtor() { }
};

struct NonTrivialCopyAssign {
  NonTrivialCopyAssign& operator=(const NonTrivialCopyAssign&) { return *this; }
  NonTrivialCopyAssign& operator=(NonTrivialCopyAssign&&) = default;
};

struct NonTrivialMoveAssign {
  NonTrivialMoveAssign& operator=(const NonTrivialMoveAssign&) = default;
  NonTrivialMoveAssign& operator=(NonTrivialMoveAssign&&) { return *this; }
};

struct NonTrivialAssign {
  NonTrivialAssign& operator=(const NonTrivialAssign&) { return *this; }
  NonTrivialAssign& operator=(NonTrivialAssign&&) { return *this; }
};

struct NonTrivialAll {
  ~NonTrivialAll() { }
  NonTrivialAll& operator=(const NonTrivialAll&) { return *this; }
  NonTrivialAll& operator=(NonTrivialAll&&) { return *this; }
};

struct ConstExpr { int i = 0; };

struct Trivial { int i; };

template<typename T>
  constexpr bool check
    = std::is_nothrow_default_constructible_v<std::optional<T>>;

// PR libstdc++/85642
static_assert(check<NonTrivialDtor>);
static_assert(check<NonTrivialCopyAssign>);
static_assert(check<NonTrivialMoveAssign>);
static_assert(check<NonTrivialAssign>);
static_assert(check<NonTrivialAll>);
static_assert(check<ConstExpr>);
static_assert(check<Trivial>);
