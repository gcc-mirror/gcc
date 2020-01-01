// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2020 Free Software Foundation, Inc.
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

#include <type_traits>
#include <chrono>

// Helper types:
struct has_type_impl
{
  template<typename T, typename = typename T::type>
  static std::true_type test(int);
  template<typename>
  static std::false_type test(...);
};

template<typename T>
struct has_type : public decltype(has_type_impl::test<T>(0))
{};

template<typename T, typename Expected>
struct is_expected_type : public std::is_same<typename T::type, Expected>
{};

template<typename P1, typename P2>
struct and_ : public std::conditional<P1::value, P2, std::false_type>::type
{};

template<typename T, typename Expected>
struct is_type : public and_<has_type<T>, is_expected_type<T, Expected>>
{};

// Inspection types:

typedef std::chrono::duration<int, std::nano> din;
typedef std::chrono::duration<double, std::nano> ddn;
typedef std::chrono::duration<int, std::milli> dim;

static_assert(is_type<std::common_type<din, din>, din>(), "");
static_assert(is_type<std::common_type<din, din, din>, din>(), "");

static_assert(is_type<std::common_type<din, ddn>, ddn>(), "");
static_assert(is_type<std::common_type<din, din, ddn>, ddn>(), "");
static_assert(is_type<std::common_type<din, ddn>, ddn>(), "");
static_assert(is_type<std::common_type<ddn, din, din>, ddn>(), "");

static_assert(!has_type<std::common_type<din, int>>(), "");
static_assert(!has_type<std::common_type<din, din, int>>(), "");
static_assert(!has_type<std::common_type<int, din, din>>(), "");

