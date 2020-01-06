// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2020 Free Software Foundation, Inc.
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

void test01() 
{
  __gnu_cxx::__vstring vs1;
  vs1.replace(vs1.cbegin(), vs1.cend(), vs1);
  vs1.replace(vs1.cbegin(), vs1.cend(), "1", 1);
  vs1.replace(vs1.cbegin(), vs1.cend(), "2");
  vs1.replace(vs1.cbegin(), vs1.cend(), 1, '3');
  vs1.replace(vs1.cbegin(), vs1.cend(), vs1.begin(), vs1.end());
  vs1.replace(vs1.cbegin(), vs1.cend(), {'4', '5'});
}
