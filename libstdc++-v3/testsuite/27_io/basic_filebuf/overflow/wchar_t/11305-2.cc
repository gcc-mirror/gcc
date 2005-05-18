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
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 27.8.1.4 Overridden virtual functions

#include <fstream>
#include <locale>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  wfilebuf fb;
  locale loc(locale("en_US.UTF-8"));
  fb.pubimbue(loc);
  fb.pubsetbuf(0, 0);
  fb.open("tmp_11305-2", ios_base::out);
  wfilebuf::int_type n1 = fb.sputc(0x20000000);
  wfilebuf::int_type n2 = fb.sputc(0x40000000);
  wfilebuf* f = fb.close();
  
  VERIFY( n1 != wfilebuf::traits_type::eof() );
  VERIFY( n2 != wfilebuf::traits_type::eof() );
  VERIFY( f != NULL );
}

int main()
{
  test02();
  return 0;
}
