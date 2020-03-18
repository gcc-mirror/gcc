// Copyright (C) 2020 Free Software Foundation, Inc.
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

// PR libstdc++/96999

#include <variant>
#include <string>

struct Foo {
public:
  explicit Foo(int) noexcept {}
  Foo(Foo &&) noexcept = default;
  Foo &operator=(Foo &&) = default;
private:
  Foo() noexcept {}
};

struct Boo {
public:
  explicit Boo(int) noexcept {}
  Boo(Boo &&) noexcept = default;
  Boo &operator=(Boo &&) = default;
private:
  Boo() noexcept {}
};


template<bool X>
std::variant<Foo, Boo> g(int v, int x) {
 return  v == 0 ? std::variant<Foo, Boo>{Foo{x}} :
                                 std::variant<Foo, Boo>{Boo{x}};
}

int main()
{
  std::variant<std::variant<Foo, Boo>, std::string> err{std::string("aaa")};
}
