// 2003-12-03  Paolo Carlini  <pcarlini@suse.de>

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

// 23.3.5.3 bitset operators

#include <bitset>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  bitset<5>      b5;
  bitset<0>      b0;
  stringstream   ss;

  ss.str("*");
  ss >> b5;
  VERIFY( ss.rdstate() == ios_base::failbit );

  ss.clear();
  ss.str("*");
  ss >> b0;
  VERIFY( ss.rdstate() == ios_base::goodbit );
}

int main()
{
  test01();
  return 0;
}
