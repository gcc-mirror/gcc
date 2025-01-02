// Copyright (C) 2017-2025 Free Software Foundation, Inc.
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

// { dg-options "-std=gnu++11" }
// { dg-do compile { target c++11 } }
// { dg-timeout-factor 2 }

#include <regex>

// Ensure compilation of trivial example still works with C++11.
// https://gcc.gnu.org/ml/libstdc++/2017-09/msg00040.html
std::regex r{""};
std::cmatch m;
bool b = regex_match("", m, r);
