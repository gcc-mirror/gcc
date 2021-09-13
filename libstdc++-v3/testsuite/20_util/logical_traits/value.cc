// { dg-do compile { target c++17 } }
//
// Copyright (C) 2015-2021 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <type_traits>

void test01()
{
  static_assert(std::negation<std::false_type>{});
  static_assert(!std::negation<std::true_type>{});
  static_assert(std::conjunction<>{});
  static_assert(!std::disjunction<>{});
  static_assert(std::conjunction<std::true_type>{});
  static_assert(!std::conjunction<std::false_type>{});
  static_assert(std::disjunction<std::true_type>{});
  static_assert(!std::disjunction<std::false_type>{});
  static_assert(std::conjunction<std::true_type, std::true_type>{});
  static_assert(!std::conjunction<std::true_type, std::false_type>{});
  static_assert(std::disjunction<std::false_type, std::true_type>{});
  static_assert(!std::disjunction<std::false_type, std::false_type>{});
  static_assert(std::conjunction<std::true_type, std::true_type,
                std::true_type>{});
  static_assert(!std::conjunction<std::true_type, std::true_type,
                std::false_type>{});
  static_assert(std::disjunction<std::false_type, std::false_type,
                std::true_type>{});
  static_assert(!std::disjunction<std::false_type, std::false_type,
                std::false_type>{});
}
