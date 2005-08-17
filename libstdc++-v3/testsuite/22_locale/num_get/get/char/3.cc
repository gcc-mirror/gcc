// { dg-require-namedlocale "" }

// 2001-11-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005 Free Software Foundation
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

// 22.2.2.1.1  num_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test03()
{
  using namespace std;
  typedef istreambuf_iterator<char> iterator_type;

  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_hk = locale("en_HK");
  VERIFY( loc_c != loc_hk );

  // sanity check the data is correct.
  const string empty;

  long l1 = 2147483647;
  long l2 = -2147483647;
  long l;

  // cache the num_get facet
  istringstream iss;
  iss.imbue(loc_hk);
  const num_get<char>& ng = use_facet<num_get<char> >(iss.getloc()); 
  const ios_base::iostate goodbit = ios_base::goodbit;
  ios_base::iostate err = ios_base::goodbit;

  // HK
  // long, in a locale that expects grouping
  iss.str("2,147,483,647 ");
  iss.clear();
  err = goodbit;
  ng.get(iss.rdbuf(), 0, iss, err, l);
  VERIFY( l == l1 );
  VERIFY( err == goodbit );

  iss.str("-2,147,483,647++++++");
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
