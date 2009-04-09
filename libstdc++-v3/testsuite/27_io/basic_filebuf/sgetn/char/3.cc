// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2009 Free Software Foundation, Inc.
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

const char name_01[] = "sgetn.txt"; 

void test06()
{
  using namespace std;

  bool test __attribute__((unused)) = true;
  char buffer[] = "xxxxxxxxxx";
  typedef filebuf::int_type	int_type;
  filebuf fbuf01;
  fbuf01.open(name_01, ios_base::in);
  int_type len1 = fbuf01.sgetn(buffer, sizeof(buffer));
  VERIFY( len1 == sizeof(buffer) );
  VERIFY( buffer[0] == '/' );
}

int main() 
{
  test06();
  return 0;
}
