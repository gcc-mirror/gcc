// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2003-05-03  Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003-2019 Free Software Foundation, Inc.
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

#include <testsuite_hooks.h>
#include <fstream>
#include <cstdio>

// libstdc++/9520
void test01()
{
  using namespace std;

  const char* name = "tmp_9520";

  FILE* file = fopen(name, "w");
  for (int i = 1; i < 256; ++i)
    putc(static_cast<unsigned char>(i), file);
  fclose(file);

  locale loc (locale(ISO_8859(15,de_DE)));
  wchar_t buf[1];
  wfilebuf fb;
  fb.pubimbue(loc);
  fb.pubsetbuf(buf, 1);
  fb.open(name, ios_base::in);

  for (int j = 1; j < 256; ++j)
    {
      wfilebuf::int_type c1 = fb.sgetc();
      VERIFY( c1 != wfilebuf::traits_type::eof() );
      wfilebuf::int_type c2 = fb.sbumpc();
      VERIFY( c1 == c2 );
    }

  fb.close();
}

int main()
{
  test01();  
  return 0;
}
