// { dg-require-namedlocale "" }

// 2001-05-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
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

// 27.7.1.3 Overridden virtual functions

#include <sstream>
#include <testsuite_hooks.h>

// libstdc++/9322
void test03()
{
  using std::locale;
  bool test __attribute__((unused)) = true;

  locale loc =  std::locale::classic();
  std::wstringbuf ob;
  VERIFY( ob.getloc() == loc );

  locale::global(locale("en_US"));
  VERIFY( ob.getloc() == loc );

  locale loc_de = locale("de_DE");
  locale ret = ob.pubimbue(loc_de);
  VERIFY( ob.getloc() == loc_de );
  VERIFY( ret == loc );

  locale::global(loc);
  VERIFY( ob.getloc() == loc_de );
}

int main() 
{
  using namespace std;
  test03();
  return 0;
}
