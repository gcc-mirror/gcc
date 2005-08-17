// { dg-require-namedlocale "" }

// Copyright (C) 2003, 2005 Free Software Foundation, Inc.
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

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <locale>
#include <testsuite_hooks.h>

void test04()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  wfilebuf fb;
  locale loc(locale("en_US.UTF-8"));
  fb.pubimbue(loc);
  fb.open("tmp_11405-4", ios_base::out);
  wfilebuf::int_type n1 = fb.sputc(0x20000000);
  wfilebuf::int_type n2 = fb.sputc(0x40000000);
  wfilebuf* f = fb.close();
  
  VERIFY( n1 != wfilebuf::traits_type::eof() );
  VERIFY( n2 != wfilebuf::traits_type::eof() );
  VERIFY( f != NULL );
}

int main()
{
  test04();
  return 0;
}
