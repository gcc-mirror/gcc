// Copyright (C) 2015-2016 Free Software Foundation, Inc.
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

#include <string>
#include <tuple>
#include <testsuite_hooks.h>

static std::string result;

struct X {
  int state; // this has to be here
  X() {
    result += "Def";
  }

  X(X const&) {
    result += "Copy";
  }

  X(X&&) {
    result += "Move";
  }

  ~X() {
    result += "Dtor";
  }
};

void f()
{
  X v;
  std::tuple<X> t1{v};
  std::tuple<std::tuple<X>&&> t2{std::move(t1)};
  std::tuple<std::tuple<X>> t3{std::move(t2)};
}

void f2()
{
  X v;
  std::tuple<X> t1{std::move(v)};
  std::tuple<std::tuple<X>&&> t2{std::move(t1)};
  std::tuple<std::tuple<X>> t3{std::move(t2)};
}

void f3()
{
  std::tuple<X> t1{X{}};
  std::tuple<std::tuple<X>&&> t2{std::move(t1)};
  std::tuple<std::tuple<X>> t3{std::move(t2)};
}

int main()
{
  f();
  VERIFY(result == "DefCopyMoveDtorDtorDtor");
  result = "";
  f2();
  VERIFY(result == "DefMoveMoveDtorDtorDtor");
  result = "";
  f3();
  VERIFY(result == "DefMoveDtorMoveDtorDtor");
  result = "";
}
