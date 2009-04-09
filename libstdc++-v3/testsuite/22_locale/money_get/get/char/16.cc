// { dg-require-namedlocale "" }

// 2004-03-02  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004, 2005, 2009 Free Software Foundation
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

// Fail as soon as value digits are not present.
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  typedef istreambuf_iterator<char> iterator_type;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = locale("de_DE@euro");
  locale loc_hk = locale("en_HK");
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_c != loc_hk );

  iterator_type end01, end02;
  istringstream iss01, iss02;
  iss01.imbue(loc_de);
  iss02.imbue(loc_hk);

  // cache the money_get facet
  const money_get<char>& mon_get_01 =
    use_facet<money_get<char> >(iss01.getloc());
  const money_get<char>& mon_get_02 =
    use_facet<money_get<char> >(iss02.getloc());

  iss01.setf(ios_base::showbase);
  iss01.str("EUR ");
  iterator_type is_it01(iss01);
  long double result1;
  ios_base::iostate err01 = ios_base::goodbit;
  end01 = mon_get_01.get(is_it01, end01, true, iss01, err01, result1);
  VERIFY( err01 == ios_base::failbit );
  VERIFY( *end01 == 'E' );
  
  iss02.str("(HKD )");
  iterator_type is_it02(iss02);
  long double result2;
  ios_base::iostate err02 = ios_base::goodbit;
  end02 = mon_get_02.get(is_it02, end02, true, iss02, err02, result2);
  VERIFY( err02 == ios_base::failbit );
  VERIFY( *end02 == ')' );
}

int main()
{
  test01();
  return 0;
}
