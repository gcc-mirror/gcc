// { dg-do compile { target c++11 } }
// 2009-11-12  Paolo Carlini  <paolo.carlini@oracle.com>
//
// Copyright (C) 2009-2019 Free Software Foundation, Inc.
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
  
template<typename From, typename To>
  struct is_convertible_mini
  {
  private:
    typedef char one;
    typedef struct { char arr[2]; } two;

    static one test(To);
    static two test(...);

  public:
    static const bool value = sizeof(test(std::declval<From>())) == 1;
};

template<typename From, typename To>
  const bool is_convertible_mini<From, To>::value;

void test01()
{
  static_assert(is_convertible_mini<int*, const int*>::value, "#1");
  static_assert(!is_convertible_mini<const void*, void*>::value, "#2");
  static_assert(is_convertible_mini<float, double>::value, "#3");
  static_assert(!is_convertible_mini<bool, int*>::value, "#4");
  static_assert(is_convertible_mini<int(&)(int), int(*)(int)>::value, "#5");
  static_assert(!is_convertible_mini<void*, int*>::value, "#6");
}
