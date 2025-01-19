// { dg-require-fileio "" }

// Copyright (C) 2010-2025 Free Software Foundation, Inc.
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

#include <fstream>
#include <cwchar>
#include <cstring>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  typedef wfilebuf::pos_type pos_type;
  const char name[] = "tmp_seekoff-4.tst";

  const size_t size = 12;
  wchar_t buf[size];
  streamsize n;

  wfilebuf fb;
  fb.open(name, ios_base::in | ios_base::out | ios_base::trunc);

  n = fb.sputn(L"abcd", 4);
  VERIFY( n == 4 );

  fb.pubseekoff(0, ios_base::beg);
  n = fb.sgetn(buf, 3);
  VERIFY( n == 3 );
  VERIFY( !wmemcmp(buf, L"abc", 3) );

  // Check read => write without pubseekoff(0, ios_base::cur)

  n = fb.sputn(L"ef", 2);
  VERIFY( n == 2 );

  fb.pubseekoff(0, ios_base::beg);

  n = fb.sgetn(buf, size);
  VERIFY( n == 5 );
  VERIFY( !wmemcmp(buf, L"abcef", 5) );

  fb.pubseekoff(0, ios_base::beg);
  n = fb.sputn(L"gh", 2);
  VERIFY( n == 2 );
  
  // Check write => read without pubseekoff(0, ios_base::cur)

  n = fb.sgetn( buf, 3 );
  VERIFY( !memcmp(buf, L"cef", 3) );

  n = fb.sputn(L"ijkl", 4);
  VERIFY( n == 4 );

  fb.pubseekoff(0, ios_base::beg);
  n = fb.sgetn(buf, 2);
  VERIFY( n == 2 );
  VERIFY( !wmemcmp(buf, L"gh", 2) );

  fb.pubseekoff(0, ios_base::end);
  n = fb.sputn(L"mno", 3);
  VERIFY( n == 3 );

  fb.pubseekoff(0, ios_base::beg);
  n = fb.sgetn(buf, size);
  VERIFY( n == 12 );
  VERIFY( !wmemcmp(buf, L"ghcefijklmno", 12) );

  fb.close();
}

int main()
{
  test01();
  return 0;
}
