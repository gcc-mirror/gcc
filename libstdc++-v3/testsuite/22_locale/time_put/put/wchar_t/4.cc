// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.5.3.1 time_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test04()
{
  using namespace std;
  typedef ostreambuf_iterator<wchar_t> iterator_type;

  bool test __attribute__((unused)) = true;
  
  // create "C" time objects
  tm time1 = { 0, 0, 12, 4, 3, 71, 0, 93, 0 };

  // basic construction and sanity checks.
  locale loc_c = locale::classic();
  locale loc_hk = __gnu_test::try_named_locale("en_HK");
  locale loc_fr = __gnu_test::try_named_locale("fr_FR@euro");
  locale loc_de = __gnu_test::try_named_locale("de_DE");
  VERIFY( loc_hk != loc_c );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  // create an ostream-derived object, cache the time_put facet
  const wstring empty;
  wostringstream oss;
  oss.imbue(loc_fr);
  const time_put<wchar_t>& tim_put = use_facet<time_put<wchar_t> >(oss.getloc()); 
  iterator_type os_it04 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'a');
  wstring result4 = oss.str();
  VERIFY( result4 == L"dim" );

  oss.str(empty); // "%d.%m.%Y"
  iterator_type os_it27 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x');
  wstring result27 = oss.str(); // "04.04.1971"
  oss.str(empty); // "%T"
  iterator_type os_it28 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X');
  wstring result28 = oss.str(); // "12:00:00"
  oss.str(empty);
  iterator_type os_it37 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'x', 'E');
  wstring result37 = oss.str(); // "04.04.1971"
  oss.str(empty);
  iterator_type os_it38 = tim_put.put(oss.rdbuf(), oss, '*', &time1, 'X', 'E');
  wstring result38 = oss.str(); // "12:00:00"
}

int main()
{
  test04();
  return 0;
}
