// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <debug_assert.h>

void test01()
{
  using namespace std;

  bool test = true;
  char buf[512];
  const char* strlit = "how to tell a story and other essays: mark twain";
  const size_t strlitsize = std::strlen(strlit);
  filebuf fbuf01;

  fbuf01.pubsetbuf(buf, 512);
  fbuf01.sputn(strlit, strlitsize);
  VERIFY( std::strncmp(strlit, buf, strlitsize) != 0 );
}

void test02()
{
  using namespace std;

  bool test = true;
  char buf[512];
  const char* strlit = "how to tell a story and other essays: mark twain";
  const size_t strlitsize = std::strlen(strlit);
  filebuf fbuf01;
  fbuf01.open("tmp", ios_base::out);

  fbuf01.pubsetbuf(buf, strlitsize);
  fbuf01.sputn(strlit, strlitsize);
  VERIFY( std::strncmp(strlit, buf, strlitsize) == 0 );
}

main() 
{
  test01();
  test02();
  return 0;
}
