// { dg-require-namedlocale "se_NO.UTF-8" }

// 2003-09-08  Petur Runolfsson  <peturr02@ru.is>

// Copyright (C) 2003-2024 Free Software Foundation, Inc.
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
#include <locale>
#include <cstdio>
#include <testsuite_hooks.h>

// Check that basic_filebuf::seekoff handles UTF-8 when open for input and
// output.
void test02()
{
  using namespace std;
  typedef wfilebuf::int_type int_type;
  const char name[] = "tmp_seekoff-2.tst";

  locale loc = locale("se_NO.UTF-8");

  const size_t size = 10;
  wchar_t buf[size];
  streamsize n;

  wfilebuf fb;
  fb.pubimbue(loc);
  fb.open(name, ios_base::in | ios_base::out | ios_base::trunc);

  n = fb.sputn(L"\xa0st", 3);
  VERIFY( n == 3 );
  
  fb.pubseekoff(0, ios_base::beg);
  n = fb.sgetn(buf, 2);
  VERIFY( n == 2 );
  VERIFY( !wmemcmp(buf, L"\xa0s", 2) );

  fb.pubseekoff(0, ios_base::cur);
  n = fb.sputn(L"\xb2R", 2);
  VERIFY( n == 2 );

  fb.pubseekoff(0, ios_base::beg);
  n = fb.sgetn(buf, size);
  VERIFY( n == 4 );
  VERIFY( !wmemcmp(buf, L"\xa0s\xb2R", 4) );

  fb.pubseekoff(0, ios_base::beg);
  n = fb.sputn(L"\x90m\x92n\x94", 5);
  VERIFY( n == 5 );

  fb.pubseekoff(0, ios_base::beg);
  n = fb.sgetn(buf, 2);
  VERIFY( n == 2 );
  VERIFY( !wmemcmp(buf, L"\x90m", 2) );

  fb.pubseekoff(0, ios_base::end);
  n = fb.sputn(L"I\xbfJ", 3);
  VERIFY( n == 3 );

  fb.pubseekoff(0, ios_base::beg);
  n = fb.sgetn(buf, size);
  VERIFY( n == 8 );
  VERIFY( !wmemcmp(buf, L"\x90m\x92n\x94I\xbfJ", 8) );

  fb.close();
}

int main()
{
  test02();
  return 0;
}
