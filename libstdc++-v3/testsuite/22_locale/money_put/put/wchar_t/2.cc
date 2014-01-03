// { dg-require-namedlocale "en_HK" }

// 2001-08-27 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2014 Free Software Foundation, Inc.
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

// test wstring version
void test02()
{
  using namespace std;
  typedef ostreambuf_iterator<wchar_t> iterator_type;

  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_hk = locale("en_HK");
  VERIFY( loc_c != loc_hk );

  // sanity check the data is correct.
  const wstring empty;

  // total EPA budget FY 2002
  const wstring digits1(L"720000000000");

  // est. cost, national missile "defense", expressed as a loss in USD 2001
  const wstring digits2(L"-10000000000000");  

  // not valid input
  const wstring digits3(L"-A"); 

  // input less than frac_digits
  const wstring digits4(L"-1");
  
  // cache the money_put facet
  wostringstream oss;
  oss.imbue(loc_hk);
  const money_put<wchar_t>& mon_put =
    use_facet<money_put<wchar_t> >(oss.getloc()); 

  // now try with showbase, to get currency symbol in format
  oss.setf(ios_base::showbase);

  // test sign of more than one digit, say hong kong.
  oss.str(empty);
  mon_put.put(oss.rdbuf(), false, oss, L' ', digits1);
  wstring result5 = oss.str();
  VERIFY( result5 == L"HK$7,200,000,000.00" );

  oss.str(empty);
  mon_put.put(oss.rdbuf(), true, oss, L' ', digits2);
  wstring result6 = oss.str();
  VERIFY( result6 == L"(HKD 100,000,000,000.00)" );

  // test one-digit formats without zero padding
  oss.imbue(loc_c);
  oss.str(empty);
  const money_put<wchar_t>& mon_put2 =
    use_facet<money_put<wchar_t> >(oss.getloc()); 
  mon_put2.put(oss.rdbuf(), true, oss, L' ', digits4);
  wstring result7 = oss.str();
  VERIFY( result7 == L"1" );

  // test one-digit formats with zero padding, zero frac widths
  oss.imbue(loc_hk);
  oss.str(empty);
  const money_put<wchar_t>& mon_put3 =
    use_facet<money_put<wchar_t> >(oss.getloc()); 
  mon_put3.put(oss.rdbuf(), true, oss, L' ', digits4);
  wstring result8 = oss.str();
  VERIFY( result8 == L"(HKD .01)" );

  oss.unsetf(ios_base::showbase);

  // test bunk input
  oss.str(empty);
  mon_put.put(oss.rdbuf(), true, oss, L' ', digits3);
  wstring result9 = oss.str();
  VERIFY( result9 == L"" );
}

int main()
{
  test02();
  return 0;
}
