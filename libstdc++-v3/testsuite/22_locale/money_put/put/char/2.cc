// { dg-require-namedlocale "en_HK.ISO8859-1" }

// 2001-08-27 Benjamin Kosnik  <bkoz@redhat.com>

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

// 22.2.6.2.1 money_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// test string version
void test02()
{
  using namespace std;
  typedef ostreambuf_iterator<char> iterator_type;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_hk = locale(ISO_8859(1,en_HK));
  VERIFY( loc_c != loc_hk );

  // sanity check the data is correct.
  const string empty;

  // total EPA budget FY 2002
  const string digits1("720000000000");

  // est. cost, national missile "defense", expressed as a loss in USD 2001
  const string digits2("-10000000000000");  

  // not valid input
  const string digits3("-A"); 

  // input less than frac_digits
  const string digits4("-1");
  
  // cache the money_put facet
  ostringstream oss;
  oss.imbue(loc_hk);
  const money_put<char>& mon_put = use_facet<money_put<char> >(oss.getloc()); 

  // now try with showbase, to get currency symbol in format
  oss.setf(ios_base::showbase);

  // test sign of more than one digit, say hong kong.
  oss.str(empty);
  mon_put.put(oss.rdbuf(), false, oss, ' ', digits1);
  string result5 = oss.str();
  VERIFY( result5 == "HK$7,200,000,000.00");

  oss.str(empty);
  mon_put.put(oss.rdbuf(), true, oss, ' ', digits2);
  string result6 = oss.str();
  VERIFY( result6 == "(HKD 100,000,000,000.00)");

  // test one-digit formats without zero padding
  oss.imbue(loc_c);
  oss.str(empty);
  const money_put<char>& mon_put2 = use_facet<money_put<char> >(oss.getloc()); 
  mon_put2.put(oss.rdbuf(), true, oss, ' ', digits4);
  string result7 = oss.str();
  VERIFY( result7 == "1");

  // test one-digit formats with zero padding, zero frac widths
  oss.imbue(loc_hk);
  oss.str(empty);
  const money_put<char>& mon_put3 = use_facet<money_put<char> >(oss.getloc()); 
  mon_put3.put(oss.rdbuf(), true, oss, ' ', digits4);
  string result8 = oss.str();
  VERIFY( result8 == "(HKD .01)");

  oss.unsetf(ios_base::showbase);

  // test bunk input
  oss.str(empty);
  mon_put.put(oss.rdbuf(), true, oss, ' ', digits3);
  string result9 = oss.str();
  VERIFY( result9 == "");
}

int main()
{
  test02();
  return 0;
}
