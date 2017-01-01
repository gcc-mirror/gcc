// { dg-do run { target c++11 } }

// Copyright (C) 2007-2017 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

using namespace std;

int
main()
{
  int x1 = 0;
  int x2 = 0;
  int y1 = 0;
  int y2 = 0;
  tuple<int,int> ta(1,1);
  tuple<const int&,const int&> tc(x1,x2);
  tie(y1,y2)=ta;
  VERIFY(y1 == 1 && y2 == 1);
  tie(y1,y2)=tc;
  VERIFY(y1 == 0 && y2 == 0);
}
