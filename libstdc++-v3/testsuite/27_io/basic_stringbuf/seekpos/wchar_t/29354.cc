// Copyright (C) 2006, 2009 Free Software Foundation, Inc.
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

#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/29354
void test01()
{
  bool test __attribute__((unused)) = true;
  using namespace std;
  typedef stringbuf::pos_type pos_type;
  typedef stringbuf::off_type off_type;

  wstringbuf strb_01(ios_base::out);

  pos_type pt_1 = strb_01.pubseekoff(0, ios_base::cur, ios_base::out);
  VERIFY( pt_1 == pos_type(off_type(0)) );  

  pos_type pt_2 = strb_01.pubseekpos(pt_1, ios_base::out);
  VERIFY( pt_2 == pos_type(off_type(0)) );
}

int
main()
{
  test01();
  return 0;
}
