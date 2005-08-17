// { dg-require-namedlocale "" }

// 2001-01-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2005 Free Software Foundation
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

// 22.2.3.1.1 nunpunct members

#include <locale>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;
  
  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_us = locale("en_US");
  locale loc_fr = locale("fr_FR");
  locale loc_de = locale("de_DE");
  VERIFY( loc_c != loc_de );
  VERIFY( loc_us != loc_fr );
  VERIFY( loc_us != loc_de );
  VERIFY( loc_de != loc_fr );

  // cache the numpunct facets
  const numpunct<char>& nump_c = use_facet<numpunct<char> >(loc_c); 
  const numpunct<char>& nump_us = use_facet<numpunct<char> >(loc_us); 
  const numpunct<char>& nump_fr = use_facet<numpunct<char> >(loc_fr); 
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

  char dp3 = nump_fr.decimal_point();
  char th3 = nump_fr.thousands_sep();
  string g3 = nump_fr.grouping();
  string t3 = nump_fr.truename();
  string f3 = nump_fr.falsename();

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
