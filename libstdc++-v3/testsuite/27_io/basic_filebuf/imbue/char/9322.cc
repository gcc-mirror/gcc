// { dg-require-namedlocale "en_US.ISO8859-1" }
// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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
#include <testsuite_hooks.h>

// libstdc++/9322
void test07()
{
  using std::locale;

  locale loc;
  std::filebuf ob;
  VERIFY( ob.getloc() == loc );

  locale::global(locale(ISO_8859(1,en_US)));
  VERIFY( ob.getloc() == loc );

  locale loc_de = locale(ISO_8859(15,de_DE));
  locale ret = ob.pubimbue(loc_de);
  VERIFY( ob.getloc() == loc_de );
  VERIFY( ret == loc );

  locale::global(loc);
  VERIFY( ob.getloc() == loc_de );
}

int main() 
{
  test07();
  return 0;
}
