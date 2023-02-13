// Copyright (C) 2019-2023 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++2a" }
// { dg-do run { target c++2a } }

#include <variant>
#include <testsuite_hooks.h>

void
test01()
{
  struct Visitor
  {
    int operator()(int, void*) const { return 0; }
    int operator()(char& c, void* p) const { return &c == p; }
    int operator()(int i, const char* s) const { return s[i] == '\0'; }
    int operator()(char c, const char* s) const { return c == *s; }
  };

  std::variant<int, char> v1{'c'};
  std::variant<void*, const char*> v2{"chars"};

  auto res = std::visit<bool>(Visitor{}, v1, v2);
  static_assert(std::is_same_v<decltype(res), bool>);
  VERIFY( res == true );

  static_assert(std::is_void_v<decltype(std::visit<void>(Visitor{}, v1, v2))>);
}

void test02()
{
  struct Visitor
  {
    int operator()(double) {return 42;}
    double operator()(int) {return 0.02;}
  };
  std::variant<int, double> v;
  std::visit<int>(Visitor(), v);
  std::visit<const void>(Visitor(), v);
}

void test03()
{
  // PR libstdc++/106589 - visit<void> rejects lambdas that do not return void
  auto visitor = []{ return 0; };
  std::visit<void>(visitor);
  std::visit<void>(static_cast<int(*)()>(visitor));
}

int
main()
{
  test01();
  test02();
  test03();
}
