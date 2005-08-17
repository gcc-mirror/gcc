// { dg-require-namedlocale "" }

// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.5.3.1 time_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test04()
{
  using namespace std;
  typedef ostreambuf_iterator<char> iterator_type;

  bool test __attribute__((unused)) = true;

  // create "C" time objects
  tm time1 = { 0, 0, 12, 4, 3, 71, 0, 93, 0 };

  // basic construction and sanity check
  locale loc_c = locale::classic();
  locale loc_fr = locale("fr_FR@euro");
  VERIFY( loc_fr != loc_c );

  // create an ostream-derived object, cache the time_put facet
  const string empty;
  ostringstream oss;
  oss.imbue(loc_fr);
  const time_put<char>& tim_put = use_facet<time_put<char> >(oss.getloc()); 
  iterator_type os_it04 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  string result4 = oss.str();
  VERIFY( result4 == "dim" );

  oss.str(empty); // "%d.%m.%Y"
  iterator_type os_it27 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x');
  string result27 = oss.str(); // "04.04.1971"
  VERIFY( result27 == "04.04.1971" );

  oss.str(empty); // "%T"
  iterator_type os_it28 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X');
  string result28 = oss.str(); // "12:00:00"
  VERIFY( result28 == "12:00:00" );

  oss.str(empty);
  iterator_type os_it37 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x', 'E');
  string result37 = oss.str(); // "04.04.1971"
  VERIFY( result37 == "04.04.1971" );

  oss.str(empty);
  iterator_type os_it38 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X', 'E');
  string result38 = oss.str(); // "12:00:00"
  VERIFY( result38 == "12:00:00" );
}

int main()
{
  test04();
  return 0;
}
