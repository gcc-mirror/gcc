// 2003-02-24 Petur Runolfsson <peturr02@ru.is>

// Copyright (C) 2003 Free Software Foundation
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

// 22.2.4.1.1 collate members

#include <locale>
#include <testsuite_hooks.h>

void test03()
{
  using namespace std;
  typedef std::collate<wchar_t>::string_type string_type;

  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = __gnu_test::try_named_locale("de_DE");
  VERIFY( loc_c != loc_de );

  // cache the collate facets
  const collate<wchar_t>& coll_c = use_facet<collate<wchar_t> >(loc_c); 
  const collate<wchar_t>& coll_de = use_facet<collate<wchar_t> >(loc_de); 

  const wchar_t* strlit1 = L"a\0a\0";
  const wchar_t* strlit2 = L"a\0b\0";
  const wchar_t* strlit3 = L"a\0\xc4\0";
  const wchar_t* strlit4 = L"a\0B\0";
  const wchar_t* strlit5 = L"aa\0";
  const wchar_t* strlit6 = L"b\0a\0";

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
