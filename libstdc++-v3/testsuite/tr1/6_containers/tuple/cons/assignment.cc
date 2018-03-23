// 2004-09-23 Chris Jefferson <chris@bubblescope.net>

// Copyright (C) 2004-2018 Free Software Foundation, Inc.
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

int
main()
{
  tuple<> ta;
  tuple<> tb;
  ta = tb;

  tuple<int> tc(1);
  tuple<int> td(0);
  td = tc;
  VERIFY(get<0>(td) == 1);

  int i=0;
  tuple<int&> te(i);
  te = tc;
  VERIFY(i == 1);

  tuple<const int&> tf(tc);

  get<0>(tc) = 2;
  VERIFY(get<0>(tf) == 2);
  tuple<double> tg;
  tg = tc;
}

