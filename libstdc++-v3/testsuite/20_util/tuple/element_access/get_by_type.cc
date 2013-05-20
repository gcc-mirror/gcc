// { dg-options "-std=gnu++1y" }

// Copyright (C) 2013 Free Software Foundation, Inc.
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
  bool test __attribute__((unused)) = true;

  int j=1;
  const int k=2;
  tuple<int,int &,const int&> a(0,j,k);
  const tuple<int,int &,const int&> b(1,j,k);
  VERIFY(get<int>(a)==0 && get<int&>(a)==1 && get<const int&>(a)==2);
  get<0>(a)=3;
  get<1>(a)=4;
  VERIFY(get<int>(a)==3 && get<int&>(a)==4);
  VERIFY(j==4);
  get<1>(b)=5;
  VERIFY(get<int>(b)==1 && get<int&>(b)==5 && get<const int&>(b)==2);
  VERIFY(j==5);
}
