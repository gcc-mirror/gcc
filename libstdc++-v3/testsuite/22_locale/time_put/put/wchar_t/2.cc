// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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
  typedef ostreambuf_iterator<wchar_t> iterator_type;

  // create "C" time objects
  const tm time1 = __gnu_test::test_tm(0, 0, 12, 4, 3, 71, 0, 93, 0);

  // basic construction and sanity check
  locale loc_c = locale::classic();
  locale loc_de = locale(ISO_8859(15,de_DE));
  VERIFY( loc_de != loc_c );

  // create an ostream-derived object, cache the time_put facet
  const wstring empty;
  wostringstream oss;
  oss.imbue(loc_de);
  const time_put<wchar_t>& tim_put
    = use_facet<time_put<wchar_t> >(oss.getloc()); 

  tim_put.put(oss.rdbuf(), oss, L'*', &time1, 'a');
  wstring result2 = oss.str();
  VERIFY( result2 == L"Son" || result2 == L"So" );

  oss.str(empty); // "%d.%m.%Y"
  tim_put.put(oss.rdbuf(), oss, L'*', &time1, 'x');
  wstring result23 = oss.str(); // "04.04.1971"
  VERIFY( result23 == L"04.04.1971" );

  oss.str(empty); // "%T"
  tim_put.put(oss.rdbuf(), oss, L'*', &time1, 'X');
  wstring result24 = oss.str(); // "12:00:00"
  VERIFY( result24 == L"12:00:00" );

  oss.str(empty);
  tim_put.put(oss.rdbuf(), oss, L'*', &time1, 'x', 'E');
  wstring result33 = oss.str(); // "04.04.1971"
  VERIFY( result33 == L"04.04.1971" );

  oss.str(empty);
  tim_put.put(oss.rdbuf(), oss, L'*', &time1, 'X', 'E');
  wstring result34 = oss.str(); // "12:00:00"
  VERIFY( result34 == L"12:00:00" );
}

int main()
{
  test02();
  return 0;
}
