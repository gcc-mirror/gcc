// { dg-require-namedlocale "nl_NL.ISO8859-15" }

// 2001-01-24 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.3.2 Template class numpunct_byname

#include <locale>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;

  // nl_NL chosen because it has no thousands separator (at this time).
  locale loc_it = locale(ISO_8859(15,nl_NL));

  const numpunct<char>& nump_it = use_facet<numpunct<char> >(loc_it); 

  string g = nump_it.grouping();

  // Ensure that grouping is empty for locales with empty thousands separator.
  VERIFY( g == "" );
}

int main()
{
  test02();
  return 0;
}
