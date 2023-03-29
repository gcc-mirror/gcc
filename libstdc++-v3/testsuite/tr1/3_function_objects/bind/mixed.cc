// { dg-do run { target c++11 } }
// 2011-11-20 Jonathan Wakely <jwakely.gcc -at- gmail.com>
//
// Copyright (C) 2011-2023 Free Software Foundation, Inc.
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

// 3.6 function object binders
#include <tr1/functional>
#include <functional>
#include <testsuite_hooks.h>

// std::tr1::bind and std::bind should work together

namespace p1 = std::placeholders;
namespace p2 = std::tr1::placeholders;

using std::multiplies;
using std::minus;

void test01()
{
  static_assert( std::is_placeholder<decltype(p2::_2)>::value == 2,
      "TR1 placeholder is a std placeholder" );
  static_assert( std::tr1::is_placeholder<decltype(p1::_1)>::value == 1,
      "std placeholder is a TR2 placeholder" );
}

void test02()
{
  auto b1 = std::bind(minus<int>(), 6, p2::_2);
  auto b2 = std::tr1::bind(minus<int>(), 6, p1::_2);

  int five = 5;
  int seven = 7;

  VERIFY( std::tr1::bind(multiplies<int>(), p1::_1, b1)(five, seven) == -5 );
  VERIFY( std::bind(multiplies<int>(), p2::_1, b2)(seven, five) == 7 );

  VERIFY( std::tr1::bind<int>(multiplies<int>(), p1::_1, b1)(five, seven) == -5 );
  VERIFY( std::bind<int>(multiplies<int>(), p2::_1, b2)(seven, five) == 7 );

  static_assert( std::is_bind_expression<decltype(b2)>::value,
      "TR1 bind expression is a std bind expression" );
  static_assert( std::tr1::is_bind_expression<decltype(b1)>::value,
      "std bind expression is a TR2 bind expression" );

  const auto c1 = b1;
  const auto c2 = b2;

  static_assert( std::is_bind_expression<decltype(c2)>::value,
      "const TR1 bind expression is a std bind expression" );
  static_assert( std::tr1::is_bind_expression<decltype(c1)>::value,
      "const std bind expression is a TR2 bind expression" );

  volatile auto v1 = b1;
  volatile auto v2 = b2;

  static_assert( std::is_bind_expression<decltype(v2)>::value,
      "volatile TR1 bind expression is a std bind expression" );
  static_assert( std::tr1::is_bind_expression<decltype(v1)>::value,
      "volatile std bind expression is a TR2 bind expression" );

  const volatile auto cv1 = b1;
  const volatile auto cv2 = b2;

  static_assert( std::is_bind_expression<decltype(cv2)>::value,
      "const volatile TR1 bind expression is a std bind expression" );
  static_assert( std::tr1::is_bind_expression<decltype(cv1)>::value,
      "const volatile std bind expression is a TR2 bind expression" );
}

void test03()
{
  auto b1 = std::bind<int>(minus<int>(), 6, p2::_2);
  auto b2 = std::tr1::bind<int>(minus<int>(), 6, p1::_2);

  int five = 5;
  int seven = 7;
  VERIFY( std::tr1::bind(multiplies<int>(), p1::_1, b1)(five, seven) == -5 );
  VERIFY( std::bind(multiplies<int>(), p2::_1, b2)(seven, five) == 7 );

  VERIFY( std::tr1::bind<int>(multiplies<int>(), p1::_1, b1)(five, seven) == -5 );
  VERIFY( std::bind<int>(multiplies<int>(), p2::_1, b2)(seven, five) == 7 );

  static_assert( std::is_bind_expression<decltype(b2)>::value,
      "TR1 bind<R> expression is a std bind expression" );
  static_assert( std::tr1::is_bind_expression<decltype(b1)>::value,
      "std bind<R> expression is a TR2 bind expression" );

  const auto c1 = b1;
  const auto c2 = b2;

  static_assert( std::is_bind_expression<decltype(c2)>::value,
      "const TR1 bind<R> expression is a std bind expression" );
  static_assert( std::tr1::is_bind_expression<decltype(c1)>::value,
      "const std bind<R> expression is a TR2 bind expression" );

  volatile auto v1 = b1;
  volatile auto v2 = b2;

  static_assert( std::is_bind_expression<decltype(v2)>::value,
      "volatile TR1 bind<R> expression is a std bind expression" );
  static_assert( std::tr1::is_bind_expression<decltype(v1)>::value,
      "volatile std bind<R> expression is a TR2 bind expression" );

  const volatile auto cv1 = b1;
  const volatile auto cv2 = b2;

  static_assert( std::is_bind_expression<decltype(cv2)>::value,
      "const volatile TR1 bind<R> expression is a std bind expression" );
  static_assert( std::tr1::is_bind_expression<decltype(cv1)>::value,
      "const volatile std bind<R> expression is a TR2 bind expression" );
}

int main()
{
  test01();
  test02();
  test03();
  return 0;
}
