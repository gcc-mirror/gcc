// { dg-do compile { target c++11 } }

// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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

#include <tuple>

typedef std::tuple<int>           Tuple_1;
typedef std::tuple<int, int>      Tuple_2;
typedef std::tuple<int, int, int> Tuple_3;

      Tuple_1 A_1() { return Tuple_1(); }
const Tuple_1 B_1() { return Tuple_1(); }

      Tuple_2 A_2() { return Tuple_2(); }
const Tuple_2 B_2() { return Tuple_2(); }

      Tuple_3 A_3() { return Tuple_3(); }
const Tuple_3 B_3() { return Tuple_3(); }

Tuple_1 test_A_1(A_1());
Tuple_1 test_B_1(B_1());

Tuple_2 test_A_2(A_2());
Tuple_2 test_B_2(B_2());

Tuple_3 test_A_3(A_3());
Tuple_3 test_B_3(B_3());
