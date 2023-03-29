// Copyright (C) 2010-2023 Free Software Foundation, Inc.
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

// 20.7.11 Function template bind

// { dg-options "-Wdeprecated-declarations" }
// { dg-do run { target c++11 } }

#include <functional>
#include <testsuite_hooks.h>

// target must be invoked with cv-quals of call wrapper

struct X
{
  int operator()() { return 0; }
  int operator()() const { return 1; }
  int operator()() volatile { return 2; }
  int operator()() const volatile { return 3; }

  int operator()(int, int, int) { return 0; }
  int operator()(int, int, int) const { return 1; }
  int operator()(int, int, int) volatile { return 2; }
  int operator()(int, int, int) const volatile { return 3; }
};

using std::placeholders::_1;
using std::placeholders::_2;

void test01()
{
  auto b0 = std::bind(X());
  VERIFY( b0() == 0 );

  const auto b1 = std::bind(X());
  VERIFY( b1() == 1 );

#if __cplusplus <= 201703L
  volatile auto b2 = std::bind(X());
  VERIFY( b2() == 2 ); // { dg-warning "deprecated" "" { target c++17_only } }

  const volatile auto b3 = std::bind(X());
  VERIFY( b3() == 3 ); // { dg-warning "deprecated" "" { target c++17_only } }
#endif
}

void test02()
{
  auto b0 = std::bind<int>(X());
  VERIFY( b0() == 0 );

  const auto b1 = std::bind<int>(X());
  VERIFY( b1() == 1 );

#if __cplusplus <= 201703L
  volatile auto b2 = std::bind<int>(X());
  VERIFY( b2() == 2 ); // { dg-warning "deprecated" "" { target c++17_only } }

  const volatile auto b3 = std::bind<int>(X());
  VERIFY( b3() == 3 ); // { dg-warning "deprecated" "" { target c++17_only } }
#endif
}

void test03()
{
  auto b0 = std::bind(X(), 0, _1, _2);
  VERIFY( b0(0, 0) == 0 );

  const auto b1 = std::bind(X(), _1, 0, _2);
  VERIFY( b1(0, 0) == 1 );

#if __cplusplus <= 201703L
  volatile auto b2 = std::bind(X(), _1, _2, 0);
  VERIFY( b2(0, 0) == 2 ); // { dg-warning "deprecated" "" { target c++17_only } }

  const volatile auto b3 = std::bind(X(), _1, 0, _2);
  VERIFY( b3(0, 0) == 3 ); // { dg-warning "deprecated" "" { target c++17_only } }
#endif
}

void test04()
{
  auto b0 = std::bind<int>(X(), 0, _1, _2);
  VERIFY( b0(0, 0) == 0 );

  const auto b1 = std::bind<int>(X(), _1, 0, _2);
  VERIFY( b1(0, 0) == 1 );

#if __cplusplus <= 201703L
  volatile auto b2 = std::bind<int>(X(), _1, _2, 0);
  VERIFY( b2(0, 0) == 2 ); // { dg-warning "deprecated" "" { target c++17_only } }

  const volatile auto b3 = std::bind<int>(X(), _1, 0, _2);
  VERIFY( b3(0, 0) == 3 ); // { dg-warning "deprecated" "" { target c++17_only } }
#endif
}


int main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
