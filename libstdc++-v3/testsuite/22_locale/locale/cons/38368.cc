// { dg-require-namedlocale "en_US.ISO8859-1" }

// Copyright (C) 2008-2018 Free Software Foundation, Inc.
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

// 22.1.1.2 locale constructors and destructors [lib.locale.cons]

#include <locale>
#include <testsuite_hooks.h>

// libstdc++/38368
void test01()
{
  using namespace std;

  locale loc(locale("C"), ISO_8859(1,en_US), locale::collate);
  locale loc_copy(loc.name().c_str());

  const moneypunct<char, true>& mpunt =
    use_facet<moneypunct<char, true> >(loc_copy);
  VERIFY( mpunt.decimal_point() == '.' );
  VERIFY( mpunt.thousands_sep() == ',' );

  const moneypunct<char, false>& mpunf =
    use_facet<moneypunct<char, false> >(loc_copy);
  VERIFY( mpunf.decimal_point() == '.' );
  VERIFY( mpunf.thousands_sep() == ',' );

  const numpunct<char>& npun = use_facet<numpunct<char> >(loc_copy);
  VERIFY( npun.decimal_point() == '.' );
  VERIFY( npun.thousands_sep() == ',' );
}

int main()
{
  test01();
  return 0;
}
