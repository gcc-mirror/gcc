// { dg-do compile { target c++11 } }
// Copyright (C) 2011-2020 Free Software Foundation, Inc.
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

// 20.6.4 function object return types [func.ret]
#include <functional>

struct X
{
    int f(int) { return 0; }
    int i;
};

void test01()
{
  typedef int (X::*mfp)(int);
  typedef int X::*mp;
  mfp m = &X::f;
  mp m2 = &X::i;
  X x = { };
  std::ref(m)(x, 1);
  std::ref(m)(&x, 1);
  int& i1 __attribute__((unused)) = std::ref(m2)(x);
  int& i2 __attribute__((unused)) = std::ref(m2)(&x);
}

int main()
{
  test01();
  return 0;
}
