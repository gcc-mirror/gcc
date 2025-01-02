// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-do compile }
// { dg-require-effective-target hosted }

// NB: Don't include any other headers in this file.
// LWG 2735. std::abs(short), std::abs(signed char) and others should return
// int instead of double in order to be compatible with C++98 and C
#include <cstdlib>

template<typename> struct is_int { };
template<> struct is_int<int> { typedef int type; };

template<typename T>
typename is_int<T>::type
do_check(T t)
{
  return T(0);
}

template<typename T>
void check()
{
  do_check(std::abs(T(0)));
}

void test()
{
  check<short>();
  check<unsigned short>();
  check<char>();
  check<signed char>();
  check<unsigned char>();
}
