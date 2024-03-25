// { dg-require-namedlocale "en_US.ISO8859-1" }
// { dg-require-namedlocale "is_IS.ISO8859-1" }
// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2001-01-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

// 22.2.3.1.1 nunpunct members

#include <locale>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_us = locale(ISO_8859(1,en_US));
  locale loc_is = locale(ISO_8859(1,is_IS));
  locale loc_de = locale(ISO_8859(15,de_DE));
  VERIFY( loc_c != loc_de );
  VERIFY( loc_us != loc_is );
  VERIFY( loc_us != loc_de );
  VERIFY( loc_de != loc_is );

  // cache the numpunct facets
  const numpunct<char>& nump_c = use_facet<numpunct<char> >(loc_c); 
  const numpunct<char>& nump_us = use_facet<numpunct<char> >(loc_us); 
  const numpunct<char>& nump_is = use_facet<numpunct<char> >(loc_is); 
  const numpunct<char>& nump_de = use_facet<numpunct<char> >(loc_de); 

  // sanity check the data is correct.
  string g1 = nump_c.grouping();
  string t1 = nump_c.truename();
  string f1 = nump_c.falsename();

  char dp2 = nump_us.decimal_point();
  char th2 = nump_us.thousands_sep();
  string g2 = nump_us.grouping();
  string t2 = nump_us.truename();
  string f2 = nump_us.falsename();

  char dp3 = nump_is.decimal_point();
  char th3 = nump_is.thousands_sep();
  string g3 = nump_is.grouping();
  string t3 = nump_is.truename();
  string f3 = nump_is.falsename();

  char dp4 = nump_de.decimal_point();
  char th4 = nump_de.thousands_sep();
  string g4 = nump_de.grouping();
  string t4 = nump_de.truename();
  string f4 = nump_de.falsename();

  VERIFY( dp2 != dp3 );
  VERIFY( th2 != th3 );

  VERIFY( dp2 != dp4 );
  VERIFY( th2 != th4 );
  // XXX This isn't actually supported right now.
  // VERIFY( t2 != t3 );
  // VERIFY( f2 != f3 );
}

int main()
{
  test02();
  return 0;
}
