// 2003-04-26 Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2020 Free Software Foundation, Inc.
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

// 27.4.2.4 ios_base static members

#include <testsuite_hooks.h>
#include <iostream>

// libstdc++/9523
void test01()
{
  using namespace std;

  int index = ios_base::xalloc();

  cin.iword(index) = 5;
  cout.iword(index) = 6;
  cerr.iword(index) = 7;
  clog.iword(index) = 8;

  ios_base::sync_with_stdio(false);

  VERIFY( cin.iword(index) == 5 );
  VERIFY( cout.iword(index) == 6 );
  VERIFY( cerr.iword(index) == 7 );
  VERIFY( clog.iword(index) == 8 );
}

int main()
{
  test01();
  return 0;
}

