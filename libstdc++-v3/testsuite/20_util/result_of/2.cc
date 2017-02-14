// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2017 Free Software Foundation, Inc.
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
#include <memory>

using std::is_same;
using std::result_of;
using std::unique_ptr;

// Taken from example in [meta.trans.other]

typedef bool (&PF1)();
typedef short (*PF2)(long);
struct S {
  operator PF2() const;
  double operator()(char, int&);
  void fn(long) const;
  char data;
};

typedef void (S::*PMF)(long) const;
typedef char S::*PMD;

static_assert( is_same<result_of<S(int)>::type, short>::value, "!");
static_assert( is_same<result_of<S&(unsigned char, int&)>::type, double>::value, "!");
static_assert( is_same<result_of<PF1()>::type, bool>::value, "!");
static_assert( is_same<result_of<PMF(unique_ptr<S>, int)>::type, void>::value, "!");
static_assert( is_same<result_of<PMD(S)>::type, char&&>::value, "!");
static_assert( is_same<result_of<PMD(const S*)>::type, const char&>::value, "!");
