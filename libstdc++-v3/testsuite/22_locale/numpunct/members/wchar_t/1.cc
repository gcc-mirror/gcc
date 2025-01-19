// 2001-01-17 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

// 22.2.3.1.1 nunpunct members

#include <locale>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;

  // basic construction
  locale loc_c = locale::classic();

  // cache the numpunct facets
  const numpunct<wchar_t>& nump_c = use_facet<numpunct<wchar_t> >(loc_c); 

  // sanity check the data is correct.
  wchar_t dp1 = nump_c.decimal_point();
  wchar_t th1 = nump_c.thousands_sep();
  string g1 = nump_c.grouping();
  wstring t1 = nump_c.truename();
  wstring f1 = nump_c.falsename();
  VERIFY ( dp1 == L'.' );
  VERIFY ( th1 == L',' );
  VERIFY ( g1 == "" );
  VERIFY ( t1 == L"true" );
  VERIFY ( f1 == L"false" );
}

int main()
{
  test01();
  return 0;
}
