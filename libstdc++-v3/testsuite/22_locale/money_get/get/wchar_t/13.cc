// { dg-require-namedlocale "" }

// 2004-02-05  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004, 2005 Free Software Foundation
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
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// 22.2.6.1.1 money_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// No thousands-sep allowed after the decimal-point.
void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  typedef istreambuf_iterator<wchar_t> iterator_type;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = locale("de_DE@euro");
  VERIFY( loc_c != loc_de );

  iterator_type end01, end02;
  wistringstream iss;
  iss.imbue(loc_de);
  // cache the money_get facet
  const money_get<wchar_t>& mon_get = use_facet<money_get<wchar_t> >(iss.getloc()); 

  iss.str(L"500,1.0 ");
  iterator_type is_it01(iss);
  long double result1;
  ios_base::iostate err01 = ios_base::goodbit;
  end01 = mon_get.get(is_it01, end01, true, iss, err01, result1);
  VERIFY( err01 == ios_base::failbit );
  VERIFY( *end01 == '.' );

  iss.str(L"500,1.0 ");
  iterator_type is_it02(iss);
  long double result2;
  ios_base::iostate err02 = ios_base::goodbit;
  end02 = mon_get.get(is_it02, end02, false, iss, err02, result2);
  VERIFY( err02 == ios_base::failbit );
  VERIFY( *end02 == '.' );
}

int main()
{
  test01();
  return 0;
}
