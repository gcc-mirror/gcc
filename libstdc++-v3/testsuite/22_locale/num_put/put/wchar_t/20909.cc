// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2005-04-08  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2005-2021 Free Software Foundation, Inc.
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

// libstdc++/20909
void test01()
{
  using namespace std;

  // A locale that expects grouping.
  locale loc_de = locale(ISO_8859(15,de_DE));

  const wstring empty;
  wstring result;

  wostringstream oss;
  oss.imbue(loc_de);
  const num_put<wchar_t>& np = use_facet<num_put<wchar_t> >(oss.getloc()); 

  double d0 = 2e20;
  double d1 = -2e20;

  oss.str(empty);
  oss.clear();
  np.put(oss.rdbuf(), oss, L'*', d0);
  result = oss.str();
  VERIFY( result == L"2e+20" );

  oss.str(empty);
  oss.clear();
  np.put(oss.rdbuf(), oss, L'*', d1);
  result = oss.str();
  VERIFY( result == L"-2e+20" );

  oss.str(empty);
  oss.clear();
  oss.setf(ios::uppercase);
  np.put(oss.rdbuf(), oss, L'*', d0);
  result = oss.str();
  VERIFY( result == L"2E+20" );

  oss.str(empty);
  oss.clear();
  oss.setf(ios::showpos);
  np.put(oss.rdbuf(), oss, L'*', d0);
  result = oss.str();
  VERIFY( result == L"+2E+20" );
}

int main()
{
  test01();
  return 0;
}


