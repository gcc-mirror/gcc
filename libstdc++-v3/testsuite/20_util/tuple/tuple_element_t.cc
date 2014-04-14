// { dg-options "-std=gnu++14" }
// { dg-do compile }

// Copyright (C) 2014 Free Software Foundation, Inc.
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

using namespace std;

struct foo
{ };

int
main()
{
  // As foo isn't constructible from anything else, this
  // lets us check if the alias is returning foo when it should
  foo q1;
  tuple_element_t<0,tuple<foo,void,int> > q2(q1);
  tuple_element_t<2,tuple<void,int,foo> > q3(q1);
}
