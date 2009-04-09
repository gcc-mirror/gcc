// { dg-require-namedlocale "" }

// 2001-11-19 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2005, 2009 Free Software Foundation
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

// Make sure that, in a locale that expects grouping, when showbase
// is true, an hexadecimal or octal zero is correctly output (the case 
// of zero is special since there is no 0x, 0 respectively, prefix)
void test05()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  // A locale that expects grouping.
  locale loc_de = locale("de_DE");

  const wstring empty;
  wstring result;

  wostringstream oss;
  oss.imbue(loc_de);
  const num_put<wchar_t>& np = use_facet<num_put<wchar_t> >(oss.getloc()); 

  long l = 0;

  oss.str(empty);
  oss.clear();
  oss.setf(ios::showbase);
  oss.setf(ios::hex, ios::basefield);
  np.put(oss.rdbuf(), oss, L'+', l);
  result = oss.str();
  VERIFY( result == L"0" );

  oss.str(empty);
  oss.clear();
  oss.setf(ios::showbase);
  oss.setf(ios::oct, ios::basefield);
  np.put(oss.rdbuf(), oss, L'+', l);
  result = oss.str();
  VERIFY( result == L"0" );
}

int main()
{
  test05();
  return 0;
}
