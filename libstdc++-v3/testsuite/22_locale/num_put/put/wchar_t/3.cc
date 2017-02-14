// { dg-require-namedlocale "en_HK.ISO8859-1" }

// 2001-11-19 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2017 Free Software Foundation, Inc.
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

// 22.2.2.2.1  num_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test03()
{
  using namespace std;
  typedef ostreambuf_iterator<wchar_t> iterator_type;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_hk = locale(ISO_8859(1,en_HK));
  VERIFY( loc_c != loc_hk );

  // sanity check the data is correct.
  const wstring empty;
  wstring result1;
  wstring result2;

  long l1 = 2147483647;
  long l2 = -2147483647;

  // cache the num_put facet
  wostringstream oss;
  oss.imbue(loc_hk);
  const num_put<wchar_t>& np = use_facet<num_put<wchar_t> >(oss.getloc()); 

  // HK
  // long, in a locale that expects grouping
  oss.str(empty);
  oss.clear();
  np.put(oss.rdbuf(), oss, L'+', l1);
  result1 = oss.str();
  VERIFY( result1 == L"2,147,483,647" );

  oss.str(empty);
  oss.clear();
  oss.width(20);
  oss.setf(ios_base::left, ios_base::adjustfield);
  np.put(oss.rdbuf(), oss, L'+', l2);
  result1 = oss.str();
  VERIFY( result1 == L"-2,147,483,647++++++" );
}

int main()
{
  test03();
  return 0;
}
