// Copyright (C) 2003 Free Software Foundation, Inc.
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// { dg-require-fileio "" }

#include <fstream>
#include <testsuite_hooks.h>

// libstdc++/9339
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  filebuf fbuf01;
  int len = 35;
  fbuf01.pubsetbuf(0, 0);
  fbuf01.open("tmp_9339", ios_base::out | ios_base::trunc);
  streamsize s1 = fbuf01.sputn("Pete Goldlust @ Carl Hammer Gallery", len);
  VERIFY( s1 == len );
  fbuf01.close();

  filebuf fbuf02;
  char buf[256];
  fbuf02.open("tmp_9339", ios_base::in);
  streamsize s2 = fbuf02.sgetn(buf, 256);
  VERIFY( s2 == len );
  fbuf02.close();
}

int main()
{
  test01();
  return 0;
}
