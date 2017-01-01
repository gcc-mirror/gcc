// { dg-do compile { target c++11 } }

// Copyright (C) 2013-2017 Free Software Foundation, Inc.
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
  __gnu_cxx::__wvstring wvs1;
  wvs1.push_back(L'1');
  wvs1.erase(wvs1.cbegin());

  __gnu_cxx::__wvstring wvs2;
  wvs2.push_back(L'2');
  wvs2.push_back(L'3');
  wvs2.erase(wvs2.cbegin(), wvs2.cend());
}
