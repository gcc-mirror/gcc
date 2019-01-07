// { dg-require-namedlocale "en_HK.ISO8859-1" }

// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2019 Free Software Foundation, Inc.
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

void test03()
{
  using namespace std;
  typedef ostreambuf_iterator<char> iterator_type;

  // create "C" time objects
  const tm time1 = __gnu_test::test_tm(0, 0, 12, 4, 3, 71, 0, 93, 0);

  // basic construction and sanity check.
  locale loc_c = locale::classic();
  locale loc_hk = locale(ISO_8859(1,en_HK));
  VERIFY( loc_hk != loc_c );

  // create an ostream-derived object, cache the time_put facet
  const string empty;
  ostringstream oss;
  oss.imbue(loc_hk);
  const time_put<char>& tim_put = use_facet<time_put<char> >(oss.getloc()); 

  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  string result3 = oss.str();
  VERIFY( result3 == "Sun" );

  oss.str(empty); // "%A, %B %d, %Y"
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x');
  string result25 = oss.str(); // "Sunday, April 04, 1971"
  VERIFY( result25 == "Sunday, April 04, 1971" );

  oss.str(empty); // "%I:%M:%S %Z"
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X');
  string result26 = oss.str(); // "12:00:00 CET" or whatever timezone
  VERIFY( result26.find("12:00:00") != string::npos );

  oss.str(empty);
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x', 'E');
  string result35 = oss.str(); // "Sunday, April 04, 1971"
  VERIFY( result35 == "Sunday, April 04, 1971" );

  oss.str(empty);
  tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X', 'E');
  string result36 = oss.str(); // "12:00:00 CET"
  VERIFY( result36.find("12:00:00") != string::npos );
}

int main()
{
  test03();
  return 0;
}
