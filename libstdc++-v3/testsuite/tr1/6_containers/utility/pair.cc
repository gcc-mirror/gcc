// 2004-09-23 Chris Jefferson <chris@bubblescope.net>

// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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

// { dg-options "-std=c++98" }

// tr1 additions to pair

#include <tr1/utility>
#include <testsuite_hooks.h>

using namespace std::tr1;
using std::pair;

struct blank_class
{ };

int
main()
{
  typedef pair<int,int> test_pair_type;
  VERIFY(tuple_size<test_pair_type>::value == 2);
  // Test if tuple_element::type returns the correct type
  blank_class blank;
  tuple_element<0, pair<blank_class, int> >::type
    blank2 __attribute__((unused)) = blank;
  tuple_element<1, pair<int ,blank_class> >::type
    blank3 __attribute__((unused)) = blank;
  pair<int,int> test_pair(1, 2);
  VERIFY(get<0>(test_pair) == 1);
  VERIFY(get<1>(test_pair) == 2);
  get<0>(test_pair) = 3;
  get<1>(test_pair) = 4;
  VERIFY(get<0>(test_pair) == 3);
  VERIFY(get<1>(test_pair) == 4);

  const pair<int,int> test_pair2(1,2);
  VERIFY(get<0>(test_pair2) == 1);
  VERIFY(get<1>(test_pair2) == 2);
}

