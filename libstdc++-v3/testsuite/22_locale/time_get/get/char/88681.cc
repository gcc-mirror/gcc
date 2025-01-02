// Copyright (C) 2019-2025 Free Software Foundation, Inc.
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

// { dg-options "-fno-inline" }
// { dg-do link { target c++11 } }

#undef _GLIBCXX_USE_CXX11_ABI
#define _GLIBCXX_USE_CXX11_ABI 0
#include <locale>
#include <sstream>

using C = char;

void
test01()
{
  using namespace std;

  locale loc_c = locale::classic();

  basic_istringstream<C> iss;
  iss.imbue(loc_c);
  const time_get<C>& tget = use_facet<time_get<C>>(iss.getloc());
  typedef istreambuf_iterator<C> iter;
  const iter end;

  tm time;
  ios_base::iostate err = ios_base::badbit;

  tget.get(iter(iss), end, iss, err, &time, 'Y');
}

int
main()
{
  test01();
}
