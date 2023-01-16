// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

const char name_06[] = "filebuf_virtuals-6.txt"; // empty file, need to create

// libstdc++/9825
// filebuf::sputbackc breaks sbumpc
void test12()
{
  using namespace std;

  filebuf fbuf;

  fbuf.open(name_06, ios_base::in|ios_base::out|ios_base::trunc);
  fbuf.sputn("crazy bees!", 11);
  fbuf.pubseekoff(0, ios_base::beg);
  fbuf.sbumpc();
  fbuf.sputbackc('x');
  filebuf::int_type c = fbuf.sbumpc();
  VERIFY( c == 'x' );
  c = fbuf.sbumpc();
  VERIFY( c == 'r' );
  c = fbuf.sbumpc();
  VERIFY( c == 'a' );
  fbuf.close();  
}

int main() 
{
  test12();
  return 0;
}
