// 2001-08-27 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307,
// USA.

// 22.2.6.2.1 money_put members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// test wstring version
void test01()
{
  using namespace std;
  typedef ostreambuf_iterator<wchar_t> iterator_type;

  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = __gnu_test::try_named_locale("de_DE@euro");
  VERIFY( loc_c != loc_de );

  // sanity check the data is correct.
  const wstring empty;

  // total EPA budget FY 2002
  const wstring digits1(L"720000000000");

  // input less than frac_digits
  const wstring digits2(L"-1");
  
  // cache the money_put facet
  wostringstream oss;
  oss.imbue(loc_de);
  const money_put<wchar_t>& mon_put = use_facet<money_put<wchar_t> >(oss.getloc()); 

  iterator_type os_it01 = mon_put.put(oss.rdbuf(), true, oss, ' ', digits1);
  wstring result1 = oss.str();
  VERIFY( result1 == L"7.200.000.000,00 ");

  oss.str(empty);
  iterator_type os_it02 = mon_put.put(oss.rdbuf(), false, oss, ' ', digits1);
  wstring result2 = oss.str();
  VERIFY( result2 == L"7.200.000.000,00 ");

  // intl and non-intl versions should be the same.
  VERIFY( result1 == result2 );

  // now try with showbase, to get currency symbol in format
  oss.setf(ios_base::showbase);

  oss.str(empty);
  iterator_type os_it03 = mon_put.put(oss.rdbuf(), true, oss, ' ', digits1);
  wstring result3 = oss.str();
  VERIFY( result3 == L"7.200.000.000,00 EUR ");

  oss.str(empty);
  iterator_type os_it04 = mon_put.put(oss.rdbuf(), false, oss, ' ', digits1);
  wstring result4 = oss.str();
  VERIFY( result4 == L"7.200.000.000,00 \x20ac");

  // intl and non-intl versions should be different.
  VERIFY( result3 != result4 );
  VERIFY( result3 != result1 );
  VERIFY( result4 != result2 );

  oss.unsetf(ios_base::showbase);

  // test io.width() > length
  // test various fill strategies
  oss.str(empty);
  oss.width(20);
  iterator_type os_it10 = mon_put.put(oss.rdbuf(), true, oss, '*', digits2);
  wstring result10 = oss.str();
  VERIFY( result10 == L"***************-,01*");

  oss.str(empty);
  oss.width(20);
  oss.setf(ios_base::internal);
  iterator_type os_it11 = mon_put.put(oss.rdbuf(), true, oss, '*', digits2);
  wstring result11 = oss.str();
  VERIFY( result11 == L"-,01****************");
}

int main()
{
  test01();
  return 0;
}
