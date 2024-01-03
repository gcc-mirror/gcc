// { dg-require-namedlocale "es_ES.ISO8859-15" }

// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

void test04()
{
  using namespace std;
  typedef ostreambuf_iterator<char> iterator_type;

  // create "C" time objects
  const tm time1 = __gnu_test::test_tm(0, 0, 12, 4, 3, 71, 0, 93, 0);

  // basic construction and sanity check
  locale loc_c = locale::classic();
  locale loc_es = locale(ISO_8859(15,es_ES));
  VERIFY( loc_es != loc_c );

  // create an ostream-derived object, cache the time_put facet
  const string empty;
  ostringstream oss;
  oss.imbue(loc_es);
  const time_put<char>& tim_put = use_facet<time_put<char> >(oss.getloc()); 
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  string result4 = oss.str();
  VERIFY( result4 == "dom" );

  oss.str(empty); // "%d/%m/%y"
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x');
  string result27 = oss.str(); // "04/04/71"
  VERIFY( result27 == "04/04/71" );

  oss.str(empty); // "%T"
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X');
  string result28 = oss.str(); // "12:00:00"
  VERIFY( result28 == "12:00:00" );

  oss.str(empty);
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x', 'E');
  string result37 = oss.str(); // "04/04/71"
  VERIFY( result37 == "04/04/71" );

  oss.str(empty);
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X', 'E');
  string result38 = oss.str(); // "12:00:00"
  VERIFY( result38 == "12:00:00" );
}

int main()
{
  test04();
  return 0;
}
