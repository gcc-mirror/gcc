// Copyright (C) 2017-2019 Free Software Foundation, Inc.
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

// { dg-options "-D__STDCPP_WANT_MATH_SPEC_FUNCS__ -D__STRICT_ANSI__" }
// { dg-do compile { target c++11 } }

#define conf_hyperg 1
#define conf_hypergf 2
#define conf_hypergl 3
#define hyperg 4
#define hypergf 5
#define hypergl 6
#include <cmath>     // PR libstdc++/82644
