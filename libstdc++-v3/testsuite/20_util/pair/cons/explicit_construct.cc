// { dg-do compile { target c++11 } }

// Copyright (C) 2015-2017 Free Software Foundation, Inc.
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

#include <utility>

struct Explicit
{
  Explicit() = default;
  explicit Explicit(int) {}
};

struct ExplicitDefault
{
  explicit ExplicitDefault() {}
};

struct ExplicitDefaultDefault
{
  explicit ExplicitDefaultDefault() = default;
};

std::pair<int, int> f1() {return {1,2};}

std::pair<Explicit, Explicit> f2() {return {1,2};} // { dg-error "explicit" }

std::pair<long, long> f3() {return std::pair<int, int>{1,2};}

std::pair<Explicit, Explicit> f4()
{
  return std::pair<int, int>{1,2};  // { dg-error "could not convert" }
}

std::pair<long, long> f5() {return {1,2};}

std::pair<int, int> v0{1,2};

std::pair<Explicit, Explicit> v1{1,2};

std::pair<Explicit, Explicit> v2 = {1,2}; // { dg-error "explicit" }

std::pair<Explicit, Explicit> v3{std::pair<int,int>{1,2}};

std::pair<Explicit, Explicit> v4 =
  std::pair<int,int>{1,2}; // { dg-error "conversion" }

std::pair<char *, char *> v5(0,0);

std::pair<long, long> v6{1,2};

std::pair<long, long> v7 = {1,2};

std::pair<long, long> v8{std::pair<int,int>{1,2}};

std::pair<long, long> v9 = std::pair<int,int>{1,2};

std::pair<Explicit, Explicit> v10{v0};

std::pair<Explicit, Explicit> v11 = v0; // { dg-error "conversion" }

std::pair<long, long> v12{v0};

std::pair<long, long> v13 = v0;

void f6(std::pair<Explicit, Explicit>) {}

void f7(std::pair<long, long>) {}

std::pair<ExplicitDefault, int> f8()
{
  return {}; // { dg-error "explicit" }
}

std::pair<ExplicitDefaultDefault, int> f9()
{
  return {}; // { dg-error "explicit" }
}

void f10(std::pair<ExplicitDefault, int>) {}

void f11(std::pair<ExplicitDefaultDefault, int>) {}

void test_arg_passing()
{
  f6(v0); // { dg-error "could not convert" }
  f6(v1);
  f6({1,2}); // { dg-error "explicit" }
  f6(std::pair<Explicit, Explicit>{});
  f6(std::pair<int, int>{}); // { dg-error "could not convert" }
  f7(v0);
  f7(v6);
  f7({1,2});
  f7(std::pair<int, int>{});
  f7(std::pair<long, long>{});
  f10({}); // { dg-error "explicit" }
  f11({}); // { dg-error "explicit" }
  f10(std::pair<ExplicitDefault, int>{});
  f11(std::pair<ExplicitDefaultDefault, int>{});
}

struct MoveOnly
{
  MoveOnly() = default;
  MoveOnly(MoveOnly&&) {}
};

struct ExplicitMoveOnly
{
  ExplicitMoveOnly() = default;
  ExplicitMoveOnly(ExplicitMoveOnly&&) {}
  explicit ExplicitMoveOnly(MoveOnly&&) {}
};

std::pair<int*, ExplicitMoveOnly> v14{0, MoveOnly{}};
std::pair<ExplicitMoveOnly, int*> v15{MoveOnly{}, 0};

std::pair<int*, ExplicitMoveOnly> v16 =
  {0, MoveOnly{}}; // { dg-error "explicit" }
std::pair<ExplicitMoveOnly, int*> v17 =
  {MoveOnly{}, 0}; // { dg-error "explicit" }
