// 2001-01-17 bkoz

// Copyright (C) 2001 Free Software Foundation
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

// 22.2.3.1.1 nunpunct members

#include <locale>
#include <debug_assert.h>

void test01()
{
  using namespace std;
  
  bool test = true;
  string str;

  // basic construction
  locale loc_c = locale::classic();
  str = loc_c.name();

  locale loc_us("en_US");
  str = loc_us.name();
  VERIFY( loc_c != loc_us );

  locale loc_fr("fr_FR");
  str = loc_fr.name();
  VERIFY( loc_c != loc_fr );

  VERIFY( loc_us != loc_fr );

  locale loc_combo(loc_us, loc_fr, locale::numeric);
  str = loc_combo.name();
  VERIFY( loc_combo != loc_fr );
  VERIFY( loc_combo != loc_us );
  VERIFY( loc_combo != loc_c );

  // cache the numpunct facets
  const numpunct<char>& nump_c = use_facet<numpunct<char> >(loc_c); 
  const numpunct<char>& nump_us = use_facet<numpunct<char> >(loc_us); 
  const numpunct<char>& nump_fr = use_facet<numpunct<char> >(loc_fr); 
  const numpunct<char>& nump_combo = use_facet<numpunct<char> >(loc_combo); 

  // sanity check the data is correct.
  char dp1 = nump_c.decimal_point();
  char th1 = nump_c.thousands_sep();
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

  char dp4 = nump_combo.decimal_point();
  char th4 = nump_combo.thousands_sep();
  string g4 = nump_combo.grouping();
  string t4 = nump_combo.truename();
  string f4 = nump_combo.falsename();
}

int main()
{
  test01();

  return 0;
}
