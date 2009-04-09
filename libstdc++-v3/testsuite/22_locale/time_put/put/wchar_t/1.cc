// 2001-09-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2009
// Free Software Foundation
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

// { dg-do run { xfail dummy_wcsftime } }

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  typedef ostreambuf_iterator<wchar_t> iterator_type;

  bool test __attribute__((unused)) = true;

  // create "C" time objects
  const tm time1 = __gnu_test::test_tm(0, 0, 12, 4, 3, 71, 0, 93, 0);

  // basic construction
  locale loc_c = locale::classic();

  // create an ostream-derived object, cache the time_put facet
  const wstring empty;
  wostringstream oss;
  oss.imbue(loc_c);
  const time_put<wchar_t>& tim_put = use_facet<time_put<wchar_t> >(oss.getloc()); 

  // 1
  // iter_type 
  // put(iter_type s, ios_base& str, char_type fill, const tm* t,
  //	 char format, char modifier = 0) const;
  oss.str(empty);
  iterator_type os_it01 = tim_put.put(oss.rdbuf(), oss, L'*', &time1, 'a');
  wstring result1 = oss.str();
  VERIFY( result1 == L"Sun" );

  oss.str(empty);
  iterator_type os_it21 = tim_put.put(oss.rdbuf(), oss, L'*', &time1, 'x');
  wstring result21 = oss.str(); // "04/04/71"
  VERIFY( result21 == L"04/04/71" );

  oss.str(empty);
  iterator_type os_it22 = tim_put.put(oss.rdbuf(), oss, L'*', &time1, 'X');
  wstring result22 = oss.str(); // "12:00:00"
  VERIFY( result22 == L"12:00:00" );

  oss.str(empty);
  iterator_type os_it31 = tim_put.put(oss.rdbuf(), oss, L'*', &time1, 'x', 'E');
  wstring result31 = oss.str(); // "04/04/71"
  VERIFY( result31 == L"04/04/71" );

  oss.str(empty);
  iterator_type os_it32 = tim_put.put(oss.rdbuf(), oss, L'*', &time1, 'X', 'E');
  wstring result32 = oss.str(); // "12:00:00"
  VERIFY( result32 == L"12:00:00" );
}

int main()
{
  test01();
  return 0;
}
