// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2001-01-24 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

// 22.2.3.2 Template class numpunct_byname

#include <locale>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  string str;

  locale loc_de = locale(ISO_8859(15,de_DE));
  str = loc_de.name();

  locale loc_byname(locale::classic(), new numpunct_byname<char>(ISO_8859(15,de_DE)));
  str = loc_byname.name();

  locale loc_c = locale::classic();

  VERIFY( loc_de != loc_byname );

  // cache the numpunct facets
  const numpunct<char>& nump_c = use_facet<numpunct<char> >(loc_c); 
  const numpunct<char>& nump_byname = use_facet<numpunct<char> >(loc_byname); 
  const numpunct<char>& nump_de = use_facet<numpunct<char> >(loc_de); 

  // sanity check that the data match
  char dp1 = nump_byname.decimal_point();
  char th1 = nump_byname.thousands_sep();
  string g1 = nump_byname.grouping();
  string t1 = nump_byname.truename();
  string f1 = nump_byname.falsename();

  char dp2 = nump_de.decimal_point();
  char th2 = nump_de.thousands_sep();
  string g2 = nump_de.grouping();
  string t2 = nump_de.truename();
  string f2 = nump_de.falsename();

  VERIFY( dp1 == dp2 );
  VERIFY( th1 == th2 );
  VERIFY( g1 == g2 );
  VERIFY( t1 == t2 );
  VERIFY( f1 == f2 );

  // ...and don't match "C"
  char dp3 = nump_c.decimal_point();
  VERIFY( dp1 != dp3 );
}

int main()
{
  test01();
  return 0;
}
