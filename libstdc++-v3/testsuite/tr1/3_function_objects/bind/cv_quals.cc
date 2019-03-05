// Copyright (C) 2011-2019 Free Software Foundation, Inc.
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

#include <tr1/functional>

struct X
{
  int operator()() const { return 0; }
  int operator()() volatile { return 1; }
  int operator()() const volatile { return 2; }
  void operator()() { };
};

void test01()
{
  static_assert( std::tr1::is_placeholder<__typeof(std::tr1::placeholders::_1)>::value,
                 "decltype(_1) is a placeholder type" );

  const auto b0 = std::tr1::bind(X());
  static_assert( std::tr1::is_bind_expression<__typeof(b0)>::value,
                 "const-qualified wrapper is a bind expression" );

  volatile auto b1 = std::tr1::bind(X());
  static_assert( std::tr1::is_bind_expression<__typeof(b1)>::value,
                 "volatile-qualified wrapper is a bind expression" );

  const volatile auto b2 = std::tr1::bind(X());
  static_assert( std::tr1::is_bind_expression<__typeof(b2)>::value,
                 "const-volatile-qualified wrapper is a bind expression" );
}

int main()
{
  test01();
  return 0;
}
