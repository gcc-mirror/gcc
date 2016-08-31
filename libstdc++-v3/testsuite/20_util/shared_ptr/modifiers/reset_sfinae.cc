// Copyright (C) 2016 Free Software Foundation, Inc.
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

#include <memory>

template<typename T, typename Args, typename = void>
  struct resettable
  : std::false_type
  { };

template<typename... T> struct type_list { };

template<typename T, typename... Args>
  using reset_result
    = decltype(std::shared_ptr<T>{}.reset(std::declval<Args>()...));

template<typename T, typename... Args>
  struct resettable<T, type_list<Args...>, reset_result<T, Args...>>
  : std::true_type
  { };

template<typename T, typename... Args>
constexpr bool can_reset()
{ return resettable<T, type_list<Args...>>::value; }

template<typename T>
struct Deleter {
  void operator()(T*) const;
};

template<typename T>
using Alloc = std::allocator<T>;

struct Base { };
struct Derived : Base { };

// Positive cases:

static_assert( can_reset<const void, void*>(),
    "void* convertible to const void*");
static_assert( can_reset<const int, int*>(),
    "int* convertible to const int*");
static_assert( can_reset<Base, Derived*>(),
    "Derived* convertible to Base*");
static_assert( can_reset<const Base, Derived*>(),
    "Derived* convertible to const Base*");

// Negative cases:

static_assert( !can_reset<int, void*>(),
    "void* not convertible to int*");
static_assert( !can_reset<int, void*, Deleter<int>>(),
    "void* not convertible to int*");
static_assert( !can_reset<int, void*, Deleter<int>, Alloc<int>>(),
    "void* not convertible to int*");

static_assert( !can_reset<int, const int*>(),
    "const int* not convertible to int*");
static_assert( !can_reset<int, const int*, Deleter<int>>(),
    "const int* not convertible to int*");
static_assert( !can_reset<int, const int*, Deleter<int>, Alloc<int>>(),
    "const int* not convertible to int*");

static_assert( !can_reset<int, long*>(),
    "long* not convertible to int*");
static_assert( !can_reset<int, long*, Deleter<int>>(),
    "long* not convertible to int*");
static_assert( !can_reset<int, long*, Deleter<int>, Alloc<int>>(),
    "long* not convertible to int*");

static_assert( !can_reset<Derived, Base*>(),
    "Base* not convertible to Derived*");
static_assert( !can_reset<Derived, Base*, Deleter<int>>(),
    "Base* not convertible to Derived*");
static_assert( !can_reset<Derived, Base*, Deleter<int>, Alloc<int>>(),
    "Base* not convertible to Derived*");
