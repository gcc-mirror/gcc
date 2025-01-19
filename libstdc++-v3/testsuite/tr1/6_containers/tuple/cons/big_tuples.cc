// 2004-09-23 Chris Jefferson <chris@bubblescope.net>

// Copyright (C) 2004-2025 Free Software Foundation, Inc.
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

#include <tr1/tuple>
#include <testsuite_hooks.h>

using namespace std::tr1;
using std::pair;

// A simple class without conversions to check some things
struct foo
{ };

void
test_constructors()
{
  int x1=0,x2=0;
  const int &z1=x1;

  // Test empty constructor
  tuple<> ta __attribute__((unused));
  tuple<int,int> tb;
  // Test construction from values
  tuple<int,int> tc(x1,x2);
  tuple<int,int&> td(x1,x2);
  tuple<const int&> te(z1);
  x1=1;
  x2=1;
  VERIFY(get<0>(td) == 0 && get<1>(td) == 1 && get<0>(te) == 1);

  // Test identical tuple copy constructor
  tuple<int,int> tf(tc);
  tuple<int,int> tg(td);
  tuple<const int&> th(te);
  // Test different tuple copy constructor
  tuple<int,double> ti(tc);
  tuple<int,double> tj(td);
  // Test constructing from a pair
  pair<int,int> pair1(1,1);
  const pair<int,int> pair2(pair1);
  tuple<int,int> tl(pair1);
  tuple<int,const int&> tm(pair1);
  tuple<int,int> tn(pair2);
  tuple<int,const int&> to(pair2);
}

int
main(void)
{
  //test construction
  typedef tuple<int,int,int,int,int,int,int,int,int,int> type1;
  type1 a(0, 0, 0, 0, 0, 0, 0, 0, 0, 1);
  type1 b(0, 0, 0, 0, 0, 0, 0, 0, 0, 2);
  type1 c(a);
  typedef tuple<int,int,int,int,int,int,int,int,int,char> type2;
  type2 d(0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
  type1 e(d);
  typedef tuple<foo,int,int,int,int,int,int,int,int,foo> type3;
  // get
  VERIFY(get<9>(a)==1 && get<9>(b)==2);
  // comparisons
  VERIFY(a==a && !(a!=a) && a<=a && a>=a && !(a<a) && !(a>a));
  VERIFY(!(a==b) && a!=b && a<=b && a<b && !(a>=b) && !(a>b));
  //tie
  {
    int i = 0;
  tie(ignore, ignore, ignore, ignore, ignore, ignore, ignore, ignore,
      ignore, i) = a;
  VERIFY(i == 1);
  }
  //test_assignment
  a=d;
  a=b;
  //make_tuple
  make_tuple(0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  //tuple_size
  VERIFY(tuple_size<type3>::value == 10);
  //tuple_element
  {
    foo q1;
    tuple_element<0,type3>::type q2(q1);
    tuple_element<9,type3>::type q3(q1);
  }

}

