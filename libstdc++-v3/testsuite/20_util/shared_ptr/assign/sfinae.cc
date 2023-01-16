// Copyright (C) 2016-2023 Free Software Foundation, Inc.
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

// { dg-do compile { target c++11 } }
// { dg-require-effective-target hosted }

#include <memory>

template<typename T, typename From>
constexpr bool can_assign()
{ return std::is_assignable<std::shared_ptr<T>, From>::value; }

struct Base { };
struct Derived : Base { };

// Positive cases:

static_assert( can_assign<const void, const std::shared_ptr<void>&>(),
    "void* convertible to const void*");
static_assert( can_assign<const void, std::shared_ptr<void>&&>(),
    "void* convertible to const void*");
static_assert( can_assign<const int, std::shared_ptr<int>>(),
    "int* convertible to const int*");
static_assert( can_assign<Base, std::shared_ptr<Derived>>(),
    "Derived* convertible to Base*");
static_assert( can_assign<const Base, std::shared_ptr<Derived>>(),
    "Derived* convertible to const Base*");

// Negative cases:

static_assert( !can_assign<int, const std::shared_ptr<void>&>(),
    "void* not convertible to int*");
static_assert( !can_assign<int, std::shared_ptr<void>&&>(),
    "void* not convertible to int*");

static_assert( !can_assign<int, const std::shared_ptr<const int>&>(),
    "const int* not convertible to int*");
static_assert( !can_assign<int, std::shared_ptr<const int>&&>(),
    "const int* not convertible to int*");

static_assert( !can_assign<int, const std::shared_ptr<long>&>(),
    "long* not convertible to int*");
static_assert( !can_assign<int, std::shared_ptr<long>&&>(),
    "long* not convertible to int*");

static_assert( !can_assign<int, std::unique_ptr<long>&&>(),
    "unique_ptr<long>::pointer not convertible to int*");

static_assert( !can_assign<Derived, const std::shared_ptr<Base>&>(),
    "Base* not convertible to Derived*");
static_assert( !can_assign<int, std::shared_ptr<long>&&>(),
    "Base* not convertible to Derived*");
static_assert( !can_assign<Derived, std::unique_ptr<Base>&&>(),
    "unique_ptr<Base>::pointer not convertible to Derived*");

struct Deleter {
  using pointer = void*;
  void operator()(pointer) const { }
};

static_assert( !can_assign<Derived, std::unique_ptr<Derived, Deleter>&&>(),
    "unique_ptr<Derived, Deleter>::pointer not convertible to Derived*");
