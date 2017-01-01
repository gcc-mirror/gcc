// 2004-10-06  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2017 Free Software Foundation, Inc.
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

#include <sstream>
#include <cstring>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  const unsigned max_size = 1 << 18;

  char ref[max_size];
  memset(ref, '\0', max_size);

  char src[max_size * 2];
  memset(src, '\1', max_size * 2);

  for (unsigned i = 128; i <= max_size; i *= 2)
    {
      char* dest = new char[i * 2];
      memset(dest, '\0', i * 2);

      stringbuf sbuf;
      sbuf.pubsetbuf(dest, i);

      sbuf.sputn(src, i * 2);
      VERIFY( !memcmp(dest + i, ref, i) );
      
      delete[] dest;
    }
}

int main()
{
  test01();
  return 0;
}
