// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2003-04-30  Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003-2018 Free Software Foundation, Inc.
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
#include <iostream>
#include <cstdio>

void test01()
{
  using namespace std;

  const char* name = "tmp_9520";

  FILE* file = fopen(name, "w");
  for (int i = 1; i < 256; ++i)
    putc(static_cast<unsigned char>(i), file);
  fclose(file);

  locale loc (locale(ISO_8859(15,de_DE)));
  locale::global(loc); // Set locale for stdin

  VERIFY( freopen(name, "r", stdin) );

  wcin.imbue(loc);

  for (int j = 1; j < 256; ++j)
    {
      wint_t c1 = wcin.rdbuf()->sgetc();
      VERIFY( c1 != WEOF );
      wint_t c2 = wcin.rdbuf()->sbumpc();
      VERIFY( c1 == c2 );
    }
}

int main()
{
  test01();  
  return 0;
}
