// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2003-02-24 Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003-2016 Free Software Foundation, Inc.
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

// 22.2.4.1.1 collate members

#include <locale>
#include <testsuite_hooks.h>

void test03()
{
  using namespace std;
  typedef std::collate<char>::string_type string_type;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = locale(ISO_8859(15,de_DE));
  VERIFY( loc_c != loc_de );

  // cache the collate facets
  const collate<char>& coll_c = use_facet<collate<char> >(loc_c); 
  const collate<char>& coll_de = use_facet<collate<char> >(loc_de); 

  const char* strlit1 = "a\0a\0";
  const char* strlit2 = "a\0b\0";
  const char* strlit3 = "a\0\xc4\0";
  const char* strlit4 = "a\0B\0";
  const char* strlit5 = "aa\0";
  const char* strlit6 = "b\0a\0";

  int i;
  string_type str1;
  string_type str2;

  str1 = coll_c.transform(strlit1, strlit1 + 3);
  str2 = coll_c.transform(strlit2, strlit2 + 3);
  i = str1.compare(str2);
  VERIFY( i < 0 );

  str1 = coll_de.transform(strlit1, strlit1 + 3);
  str2 = coll_de.transform(strlit2, strlit2 + 3);
  i = str1.compare(str2);
  VERIFY( i < 0 );

  str1 = coll_c.transform(strlit3, strlit3 + 3);
  str2 = coll_c.transform(strlit4, strlit4 + 3);
  i = str1.compare(str2);
  VERIFY( i > 0 );

  str1 = coll_de.transform(strlit3, strlit3 + 3);
  str2 = coll_de.transform(strlit4, strlit4 + 3);
  i = str1.compare(str2);
  VERIFY( i < 0 );

  str1 = coll_c.transform(strlit1, strlit1 + 1);
  str2 = coll_c.transform(strlit5, strlit5 + 1);
  i = str1.compare(str2);
  VERIFY( i == 0 );

  str1 = coll_de.transform(strlit6, strlit6 + 3);
  str2 = coll_de.transform(strlit1, strlit1 + 3);
  i = str1.compare(str2);
  VERIFY( i > 0 );

  str1 = coll_c.transform(strlit1, strlit1 + 3);
  str2 = coll_c.transform(strlit5, strlit5 + 3);
  i = str1.compare(str2);
  VERIFY( i < 0 );
}

int main()
{
  test03();
  return 0;
}
