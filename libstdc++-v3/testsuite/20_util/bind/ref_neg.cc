// Copyright (C) 2010-2021 Free Software Foundation, Inc.
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

// 20.8.9 Function template bind

// { dg-options "-fno-show-column" }
// { dg-do compile { target c++11 } }

#include <functional>

using namespace std::placeholders;

int inc(int& i) { return ++i; }

void test01()
{
  const int dummy = 0;
  std::bind(&inc, _1)(0);               // { dg-error  "no match" }
  std::bind(&inc, std::ref(dummy))();	// { dg-error  "no match" }
}

struct Inc
{
  int operator()(int& i) const { return ++i; }
  void operator()(int&&) const { }

  int f(int&& i) const { return ++i; }
};

void test02()
{
  const int dummy = 0;
  std::bind(Inc(), _1)(dummy);                  // { dg-error  "no match" }
  std::bind(&Inc::f, Inc(), std::ref(dummy))(); // { dg-error  "no match" }
}

// Ignore the reasons for deduction/substitution failure in the headers.
// Arrange for the match to work on installed trees as well as build trees.
// { dg-prune-output "/(functional|bits/invoke.h):" }

int main()
{
  test01();
  test02();
}
