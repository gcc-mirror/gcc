// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2004-02-05  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2023 Free Software Foundation, Inc.
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

// 22.2.6.1.1 money_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// Same as 3.cc but no thousands-sep in input: they are always optional.
void test01()
{
  using namespace std;

  typedef istreambuf_iterator<char> iterator_type;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = locale(ISO_8859(15,de_DE));
  VERIFY( loc_c != loc_de );

  // total EPA budget FY 2002
  const long double  digits1 = 720000000000.0;

  iterator_type end;
  istringstream iss;
  iss.imbue(loc_de);
  // cache the money_get facet
  const money_get<char>& mon_get = use_facet<money_get<char> >(iss.getloc()); 

  iss.str("7200000000,00 ");
  iterator_type is_it01(iss);
  long double result1;
  ios_base::iostate err01 = ios_base::goodbit;
  mon_get.get(is_it01, end, true, iss, err01, result1);
  VERIFY( result1 == digits1 );
  VERIFY( err01 == ios_base::eofbit );

  iss.str("7200000000,00 ");
  iterator_type is_it02(iss);
  long double result2;
  ios_base::iostate err02 = ios_base::goodbit;
  mon_get.get(is_it02, end, false, iss, err02, result2);
  VERIFY( result2 == digits1 );
  VERIFY( err02 == ios_base::eofbit );
}

int main()
{
  test01();
  return 0;
}
