// 2001-08-15 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.4.2 Template class collate_byname

#include <locale>
#include <testsuite_hooks.h>

// XXX This test is not working for non-glibc locale models.
// { dg-do run { xfail *-*-* } }

void test01()
{
  using namespace std;
  typedef std::collate<char>::string_type string_type;

  bool test = true;
  string str;
  locale loc_c = locale::classic();

  locale loc_byname(locale::classic(), new collate_byname<char>("es_ES"));
  str = loc_byname.name();

  locale loc_es("es_ES");
  str = loc_es.name();

  VERIFY( loc_es != loc_byname );

  // cache the collate facets
  const collate<char>& mssg_byname = use_facet<collate<char> >(loc_byname); 
  const collate<char>& mssg_de = use_facet<collate<char> >(loc_es); 

#if 0
  // Check Spanish (es_ES) locale.
  catalog cat_de = mssg_de.open("libstdc++", loc_c, dir);
  string s01 = mssg_de.get(cat_de, 0, 0, "please");
  string s02 = mssg_de.get(cat_de, 0, 0, "thank you");
  VERIFY ( s01 == "bitte" );
  VERIFY ( s02 == "danke" );
  mssg_de.close(cat_de);

  // Check byname locale.
  catalog cat_byname = mssg_byname.open("libstdc++", loc_c, dir);
  string s03 = mssg_byname.get(cat_de, 0, 0, "please");
  string s04 = mssg_byname.get(cat_de, 0, 0, "thank you");
  VERIFY ( s03 == "bitte" );
  VERIFY ( s04 == "danke" );
  mssg_byname.close(cat_byname);

  VERIFY ( s01 == s03 );
  VERIFY ( s02 == s04 );
#endif
}

int main()
{
  test01();

  return 0;
}
