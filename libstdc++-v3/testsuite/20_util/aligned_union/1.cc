// { dg-do compile { target c++11 } }

// 2014-04-16 Rüdiger Sonderfeld  <ruediger@c-plusplus.de>

// Copyright (C) 2014-2016 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the terms
// of the GNU General Public License as published by the Free Software
// Foundation; either version 3, or (at your option) any later
// version.

// This library is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// C++11 [meta.trans.other] 20.9.7.6: aligned_union

#include <type_traits>
#include <testsuite_tr1.h>

struct MSAlignType { } __attribute__((__aligned__));

template<typename...T>
  struct mymax
  {
    static const std::size_t alignment = 0;
    static const std::size_t size = 0;
  };

template<typename L,  typename...T>
  struct mymax<L, T...>
  {
    static const std::size_t alignment = alignof(L) > mymax<T...>::alignment
      ? alignof(L) : mymax<T...>::alignment;
    static const std::size_t size = sizeof(L) > mymax<T...>::size
      ? sizeof(L) : mymax<T...>::size;
  };

void test01()
{
  using std::aligned_union;
  using std::alignment_of;
  using std::size_t;
  using namespace __gnu_test;

  const size_t max_a = mymax<char, short, int, double, int[4],
                             ClassType, MSAlignType>::alignment;
  const size_t max_s = mymax<char, short, int, double, int[4],
                             ClassType, MSAlignType>::size;

  typedef aligned_union<0, char, short, int, double, int[4],
                        ClassType, MSAlignType> au_type;
  static_assert(au_type::alignment_value == max_a, "Alignment value");
  static_assert(sizeof(au_type::type) >= max_s, "Storage size");

  typedef aligned_union<max_s+100, char, short, int, double, int[4],
                        ClassType, MSAlignType> au_type2;
  static_assert(sizeof(au_type2::type) >= max_s+100,
                "Storage size (at least len)");
}

int main()
{
  test01();
}
