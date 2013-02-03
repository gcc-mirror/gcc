// 2003-06-02 Paolo Carlini <pcarlini@unitus.it>

// Copyright (C) 2003-2013 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

const char name_01[] = "filebuf_virtuals-1.txt"; // file with data in it

// libstdc++/9761
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  filebuf fbuf;
  filebuf::int_type r1, r2;

  fbuf.open(name_01, ios_base::in);
  
  fbuf.sbumpc();
  fbuf.sbumpc();
	
  r1 = fbuf.sputbackc('a');
  r2 = fbuf.sputbackc('b');

  fbuf.close();

  VERIFY( r1 != filebuf::traits_type::eof() );
  VERIFY( r2 == filebuf::traits_type::eof() );
}

int main() 
{
  test01();
  return 0;
}
