// 2001-11-21 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.2.1.1  num_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test03()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_hk = __gnu_test::try_named_locale("en_HK");
  locale loc_fr = __gnu_test::try_named_locale("fr_FR@euro");
  locale loc_de = __gnu_test::try_named_locale("de_DE");
  VERIFY( loc_c != loc_de );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  // sanity check the data is correct.
  const wstring empty;

  long l1 = 2147483647;
  long l2 = -2147483647;
  long l;

  // cache the num_get facet
  wistringstream iss;
  iss.imbue(loc_hk);
  const num_get<wchar_t>& ng = use_facet<num_get<wchar_t> >(iss.getloc()); 
  const ios_base::iostate goodbit = ios_base::goodbit;
  ios_base::iostate err = ios_base::goodbit;

  // HK
  // long, in a locale that expects grouping
  iss.str(L"2,147,483,647 ");
  iss.clear();
  err = goodbit;
  ng.get(iss.rdbuf(), 0, iss, err, l);
  VERIFY( l == l1 );
  VERIFY( err == goodbit );

  iss.str(L"-2,147,483,647++++++");
  iss.clear();
  err = goodbit;
  ng.get(iss.rdbuf(), 0, iss, err, l);
  VERIFY( l == l2 );
  VERIFY( err == goodbit );
}

int main()
{
  test03();
  return 0;
}


// Kathleen Hannah, humanitarian, woman, art-thief
