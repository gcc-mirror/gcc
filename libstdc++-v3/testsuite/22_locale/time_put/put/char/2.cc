// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2021 Free Software Foundation, Inc.
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

// 22.2.5.3.1 time_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;
  typedef ostreambuf_iterator<char> iterator_type;

  // create "C" time object
  const tm time1 = __gnu_test::test_tm(0, 0, 12, 4, 3, 71, 0, 93, 0);

  // basic construction and sanity check
  locale loc_c = locale::classic();
  locale loc_de = locale(ISO_8859(15,de_DE));
  VERIFY( loc_de != loc_c );

  // create an ostream-derived object, cache the time_put facet
  const string empty;
  ostringstream oss;
  oss.imbue(loc_de);
  const time_put<char>& tim_put = use_facet<time_put<char> >(oss.getloc()); 

  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  string result2 = oss.str();
  VERIFY( result2 == "Son" || result2 == "So" );

  oss.str(empty); // "%d.%m.%Y"
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x');
  string result23 = oss.str(); // "04.04.1971"
  VERIFY( result23 == "04.04.1971" );

  oss.str(empty); // "%T"
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X');
  string result24 = oss.str(); // "12:00:00"
  VERIFY( result24 == "12:00:00" );

  oss.str(empty);
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x', 'E');
  string result33 = oss.str(); // "04.04.1971"
  VERIFY( result33 == "04.04.1971" );

  oss.str(empty);
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X', 'E');
  string result34 = oss.str(); // "12:00:00"
  VERIFY( result34 == "12:00:00" );
}

int main()
{
  test02();
  return 0;
}
