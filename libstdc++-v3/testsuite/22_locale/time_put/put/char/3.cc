// { dg-require-namedlocale "" }

// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation
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

// 22.2.5.3.1 time_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test03()
{
  using namespace std;
  typedef ostreambuf_iterator<char> iterator_type;

  bool test __attribute__((unused)) = true;

  // create "C" time objects
  const tm time1 = __gnu_test::test_tm(0, 0, 12, 4, 3, 71, 0, 93, 0);

  // basic construction and sanity check.
  locale loc_c = locale::classic();
  locale loc_hk = locale("en_HK");
  VERIFY( loc_hk != loc_c );

  // create an ostream-derived object, cache the time_put facet
  const string empty;
  ostringstream oss;
  oss.imbue(loc_hk);
  const time_put<char>& tim_put = use_facet<time_put<char> >(oss.getloc()); 

  iterator_type os_it03 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  string result3 = oss.str();
  VERIFY( result3 == "Sun" );

  oss.str(empty); // "%A, %B %d, %Y"
  iterator_type os_it25 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x');
  string result25 = oss.str(); // "Sunday, April 04, 1971"
  VERIFY( result25 == "Sunday, April 04, 1971" );

  oss.str(empty); // "%I:%M:%S %Z"
  iterator_type os_it26 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X');
  string result26 = oss.str(); // "12:00:00 CET" or whatever timezone
  VERIFY( result26.find("12:00:00") != string::npos );

  oss.str(empty);
  iterator_type os_it35 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x', 'E');
  string result35 = oss.str(); // "Sunday, April 04, 1971"
  VERIFY( result35 == "Sunday, April 04, 1971" );

  oss.str(empty);
  iterator_type os_it36 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X', 'E');
  string result36 = oss.str(); // "12:00:00 CET"
  VERIFY( result36.find("12:00:00") != string::npos );
}

int main()
{
  test03();
  return 0;
}
