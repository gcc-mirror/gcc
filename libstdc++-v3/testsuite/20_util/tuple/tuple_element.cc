// { dg-do compile { target c++11 } }

// Copyright (C) 2007-2018 Free Software Foundation, Inc.
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

// Tuple

#include <tuple>

using namespace std;

struct foo
{ };

int
main()
{
  // As foo isn't constructible from anything else, this
  // lets us check if type is returning foo when it should
  foo q1;
  tuple_element<0,tuple<foo,void,int> >::type q2(q1);
  tuple_element<2,tuple<void,int,foo> >::type q3(q1);
  tuple_element<0,const tuple<foo,void,int> >::type q4(q1);
  tuple_element<2,const tuple<void,int,foo> >::type q5(q1);
  tuple_element<0,volatile tuple<foo,void,int> >::type q6(q1);
  tuple_element<2,volatile tuple<void,int,foo> >::type q7(q1);
  tuple_element<0,const volatile tuple<foo,void,int> >::type q8(q1);
  tuple_element<2,const volatile tuple<void,int,foo> >::type q9(q1);
}
