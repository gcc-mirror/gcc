// 2000-06-29 bkoz

// Copyright (C) 2000, 2003 Free Software Foundation
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

// 27.6.2.4 basic_ostream seek members

#include <ostream>
#include <sstream>
#include <fstream>
#include <testsuite_hooks.h>


void test01()
{
  using namespace std;
  typedef ios::pos_type pos_type;

  bool test __attribute__((unused)) = true;
  const char str_lit01[] = "ostream_seeks-1.txt";

  // out
  // test default ctors leave things in the same positions...
  ostringstream ost1;
  pos_type p1 = ost1.tellp();

  ofstream ofs1;
  pos_type p2 = ofs1.tellp();

  VERIFY( p1 == p2 );

  // out
  // test ctors leave things in the same positions...
  ostringstream ost2("bob_marley:kaya");
  p1 = ost2.tellp();

  ofstream ofs2(str_lit01);
  p2 = ofs2.tellp();
 
  VERIFY( p1 == p2 );
}

int main()
{
  test01();
  return 0;
}
