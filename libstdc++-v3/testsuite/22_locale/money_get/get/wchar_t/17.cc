// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2004-03-08  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004-2019 Free Software Foundation, Inc.
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

// 22.2.6.3, p2: "The value _space_ indicates that at least one space
//                is required at that position."
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = locale(ISO_8859(15,de_DE));
  VERIFY( loc_c != loc_de );

  iterator_type end, end02;
  wistringstream iss;
  iss.imbue(loc_de);
  // cache the money_get facet
  const money_get<wchar_t>& mon_get =
    use_facet<money_get<wchar_t> >(iss.getloc()); 

  iss.str(L"7.200.000.000,00");
  iterator_type is_it01(iss);
  wstring result1;
  ios_base::iostate err01 = ios_base::goodbit;
  mon_get.get(is_it01, end, true, iss, err01, result1);
  VERIFY( err01 == (ios_base::failbit | ios_base::eofbit) );

  // now try with showbase, to get currency symbol in format
  iss.setf(ios_base::showbase);

  iss.str(L"7.200.000.000,00EUR ");
  iterator_type is_it02(iss);
  wstring result2;
  ios_base::iostate err02 = ios_base::goodbit;
  end02 = mon_get.get(is_it02, end, true, iss, err02, result2);
  VERIFY( err02 == ios_base::failbit );
  VERIFY( *end02 == L'E' );
}

int main()
{
  test01();
  return 0;
}
