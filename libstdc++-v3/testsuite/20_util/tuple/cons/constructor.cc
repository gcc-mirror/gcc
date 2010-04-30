// { dg-options "-std=gnu++0x" }

// Copyright (C) 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
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
#include <utility> // for pair
#include <testsuite_hooks.h>

using namespace std;

int
main()
{
  bool test __attribute__((unused)) = true;

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
  //tuple<int&, int&> tk(tc);
  tuple<const int&, const int&> tl(tc);
  tuple<const int&, const int&> tm(tl);
  // Test constructing from a pair
  pair<int,int> pair1(1,1);
  const pair<int,int> pair2(pair1);
  tuple<int,int> tn(pair1);
  tuple<int,const int&> to(pair1);
  tuple<int,int> tp(pair2);
  tuple<int,const int&> tq(pair2);  
  return 0;
}
