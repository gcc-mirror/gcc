// 2000-06-29 bkoz

// Copyright (C) 2000, 2001, 2002, 2003 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.6.1.3 unformatted input functions
// NB: ostream has a particular "seeks" category. Adopt this for istreams too.
// @require@ %-*.tst %-*.txt
// @diff@ %-*.tst %-*.txt

#include <istream>
#include <sstream>
#include <fstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  typedef ios::pos_type pos_type;

  bool test = true;
  const char str_lit01[] = "istream_seeks-1.tst";

  // in
  // test default ctors leave things in the same positions...
  istringstream ist1;
  pos_type p3 = ist1.tellg();

  ifstream ifs1;
  pos_type p4 = ifs1.tellg();

  VERIFY( p3 == p4 );

  // in
  // test ctors leave things in the same positions...
  istringstream ist2("bob_marley:kaya");
  p3 = ist2.tellg();

  ifstream ifs2(str_lit01);
  p4 = ifs2.tellg();

  VERIFY( p3 == p4 );
}

int main()
{
  test01();
  return 0;
}
