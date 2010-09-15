// { dg-require-namedlocale "is_IS" }

// 2001-01-19 Benjamin Kosnik <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
// Free Software Foundation
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

// 22.1.1 - Class locale [lib.locale]

#include <locale>
#include <string>
#include <testsuite_hooks.h>

void
test02()
{
  using namespace std;
  bool test __attribute__((unused)) = true;
  const string name_c("C");
  const string name_no("*");
  string str;

  // construct a locale object with the specialized facet.
  locale		loc_c = locale::classic();
  locale		loc_is = locale("is_IS");
  locale 		loc_1(locale::classic(), 
			      new numpunct_byname<char>("is_IS"));

  // check names
  VERIFY( loc_c.name() == name_c );
  VERIFY( loc_1.name() == name_no );

  // sanity check the constructed locale has the specialized facet.
  VERIFY( has_facet<numpunct<char> >(loc_1) );
  VERIFY( has_facet<numpunct<char> >(loc_c) );
  
  // attempt to re-synthesize classic locale
  locale		loc_2 = loc_1.combine<numpunct<char> >(loc_c);
  VERIFY( loc_2.name() == name_no );
  VERIFY( loc_2 != loc_c );

  // extract facet
  const numpunct<char>&	nump_1 = use_facet<numpunct<char> >(loc_1); 
  const numpunct<char>&	nump_2 = use_facet<numpunct<char> >(loc_2); 
  const numpunct<char>&	nump_c = use_facet<numpunct<char> >(loc_c); 
  const numpunct<char>&	nump_is = use_facet<numpunct<char> >(loc_is); 

  // sanity check the data is correct.
  char dp1 = nump_c.decimal_point();
  char th1 = nump_c.thousands_sep();
  string g1 = nump_c.grouping();
  string t1 = nump_c.truename();
  string f1 = nump_c.falsename();

  char dp2 = nump_1.decimal_point();
  char th2 = nump_1.thousands_sep();
  string g2 = nump_1.grouping();
  string t2 = nump_1.truename();
  string f2 = nump_1.falsename();

  char dp3 = nump_2.decimal_point();
  char th3 = nump_2.thousands_sep();
  string g3 = nump_2.grouping();
  string t3 = nump_2.truename();
  string f3 = nump_2.falsename();

  char dp4 = nump_is.decimal_point();
  char th4 = nump_is.thousands_sep();
  string g4 = nump_is.grouping();
  string t4 = nump_is.truename();
  string f4 = nump_is.falsename();
  VERIFY( dp1 != dp2 );
  VERIFY( th1 != th2 );

  VERIFY( dp1 == dp3 );
  VERIFY( th1 == th3 );
  VERIFY( t1 == t3 );
  VERIFY( f1 == f3 );

  VERIFY( dp2 == dp4 );
  VERIFY( th2 == th4 );
  VERIFY( t2 == t4 );
  VERIFY( f2 == f4 );
}


int main()
{
  test02();
  return 0;
}
