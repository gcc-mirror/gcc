// 2004-10-06  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2020 Free Software Foundation, Inc.
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

// { dg-options "-DMAX_SIZE=4096" { target simulator } }

#ifndef MAX_SIZE
#define MAX_SIZE (1 << 18)
#endif

#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  const unsigned max_size = MAX_SIZE;

  static wchar_t ref[max_size];
  wmemset(ref, L'\0', max_size);

  static wchar_t src[max_size * 2];
  wmemset(src, L'\1', max_size * 2);

  for (unsigned i = 128; i <= max_size; i *= 2)
    {
      wchar_t* dest = new wchar_t[i * 2];
      wmemset(dest, L'\0', i * 2);

      wstringbuf sbuf;
      sbuf.pubsetbuf(dest, i);

      sbuf.sputn(src, i * 2);
      VERIFY( !wmemcmp(dest + i, ref, i) );
      
      delete[] dest;
    }
}

int main()
{
  test01();
  return 0;
}
