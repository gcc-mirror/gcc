// 2001-08-15 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation
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

// { dg-do run }
// { dg-options "-finput-charset=ISO8859-1" }

#include <locale>
#include <testsuite_hooks.h>

// Check German "de_DE" locale.
void test02()
{
  using namespace std;
  typedef std::collate<wchar_t>::string_type string_type;
  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_us = __gnu_test::try_named_locale("en_US");
  locale loc_fr = __gnu_test::try_named_locale("fr_FR");
  locale loc_de = __gnu_test::try_named_locale("de_DE");
  VERIFY( loc_c != loc_de );
  VERIFY( loc_us != loc_fr );
  VERIFY( loc_us != loc_de );
  VERIFY( loc_de != loc_fr );

  // cache the collate facets
  const collate<wchar_t>& coll_de = use_facet<collate<wchar_t> >(loc_de); 

  // long hash(const charT*, const charT*) cosnt
  const wchar_t* strlit3 = L"Äuglein Augment"; // "C" == "Augment Äuglein"
  const wchar_t* strlit4 = L"Base baß Baß Bast"; // "C" == "Base baß Baß Bast"

  long l1;
  long l2;
  int size3 = char_traits<wchar_t>::length(strlit4) - 1;
  int size4 = char_traits<wchar_t>::length(strlit3) - 1;

  l1 = coll_de.hash(strlit3, strlit3 + size3);
  l2 = coll_de.hash(strlit3, strlit3 + size3 - 1);
  VERIFY ( l1 != l2 );
  l1 = coll_de.hash(strlit3, strlit3 + size3);
  l2 = coll_de.hash(strlit4, strlit4 + size4);
  VERIFY ( l1 != l2 );
}

int main()
{
  test02();
  return 0;
}
