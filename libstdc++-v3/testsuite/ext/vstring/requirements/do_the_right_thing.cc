// { dg-do compile { target c++11 } }

// Copyright (C) 2012-2021 Free Software Foundation, Inc.
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

#include <ext/vstring.h>

// libstdc++/43813
void test01()
{
  __gnu_cxx::__versa_string<double*> vs(7, 0);
  vs.assign(7, 0);
  vs.append(7, 0);
  vs.insert(vs.begin(), 7, 0);
  vs.replace(vs.begin(), vs.end(), 7, 0);
}
