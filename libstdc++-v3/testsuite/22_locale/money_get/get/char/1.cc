// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2001-09-12 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

// test string version
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<char> iterator_type;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = locale(ISO_8859(15,de_DE));
  VERIFY( loc_c != loc_de );

  // sanity check the data is correct.
  const string empty;

  // total EPA budget FY 2002
  const string digits1("720000000000");

  iterator_type end;
  istringstream iss;
  iss.imbue(loc_de);
  // cache the money_get facet
  const money_get<char>& mon_get = use_facet<money_get<char> >(iss.getloc()); 

  iss.str("7.200.000.000,00 ");
  iterator_type is_it01(iss);
  string result1;
  ios_base::iostate err01 = ios_base::goodbit;
  mon_get.get(is_it01, end, true, iss, err01, result1); // xxx
  VERIFY( result1 == digits1 );
  VERIFY( err01 == ios_base::eofbit );

  iss.str("7.200.000.000,00  ");
  iterator_type is_it02(iss);
  string result2;
  ios_base::iostate err02 = ios_base::goodbit;
  mon_get.get(is_it02, end, true, iss, err02, result2);
  VERIFY( result2 == digits1 );
  VERIFY( err02 == ios_base::eofbit );

  iss.str("7.200.000.000,00  a");
  iterator_type is_it03(iss);
  string result3;
  ios_base::iostate err03 = ios_base::goodbit;
  mon_get.get(is_it03, end, true, iss, err03, result3);
  VERIFY( result3 == digits1 );
  VERIFY( err03 == ios_base::goodbit );

  iss.str("");
  iterator_type is_it04(iss);
  string result4;
  ios_base::iostate err04 = ios_base::goodbit;
  mon_get.get(is_it04, end, true, iss, err04, result4);
  VERIFY( result4 == empty );
  VERIFY( err04 == (ios_base::failbit | ios_base::eofbit) );

  iss.str("working for enlightenment and peace in a mad world");
  iterator_type is_it05(iss);
  string result5;
  ios_base::iostate err05 = ios_base::goodbit;
  mon_get.get(is_it05, end, true, iss, err05, result5);
  VERIFY( result5 == empty );
  VERIFY( err05 == ios_base::failbit );

  // now try with showbase, to get currency symbol in format
  iss.setf(ios_base::showbase);

  iss.str("7.200.000.000,00 EUR ");
  iterator_type is_it06(iss);
  string result6;
  ios_base::iostate err06 = ios_base::goodbit;
  mon_get.get(is_it06, end, true, iss, err06, result6);
  VERIFY( result6 == digits1 );
  VERIFY( err06 == ios_base::eofbit );

  iss.str("7.200.000.000,00 EUR  "); // Extra space.
  iterator_type is_it07(iss);
  string result7;
  ios_base::iostate err07 = ios_base::goodbit;
  mon_get.get(is_it07, end, true, iss, err07, result7);
  VERIFY( result7 == digits1 );
  VERIFY( err07 == ios_base::goodbit );

  iss.str("7.200.000.000,00 \244"); 
  iterator_type is_it08(iss);
  string result8;
  ios_base::iostate err08 = ios_base::goodbit;
  mon_get.get(is_it08, end, false, iss, err08, result8);
  VERIFY( result8 == digits1 );
  VERIFY( err08 == ios_base::eofbit );
}

int main()
{
  test01();
  return 0;
}
