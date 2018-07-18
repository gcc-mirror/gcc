// { dg-require-namedlocale "de_DE.ISO8859-15" }

// 2003-10-24  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2003-2018 Free Software Foundation, Inc.
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
#include <limits>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;
  
  // basic construction
  locale loc_de = locale(ISO_8859(15,de_DE));

  iterator_type end;
  wistringstream iss;
  iss.imbue(loc_de);

  // cache the money_get facet
  const money_get<wchar_t>& mon_get = use_facet<money_get<wchar_t> >(iss.getloc()); 

  // A _very_ big amount.
  wstring str = L"1";
  for (int i = 0; i < 2 * numeric_limits<long double>::digits10; ++i)
    str += L".000";
  str += L",00 ";

  iss.str(str);
  iterator_type is_it01(iss);
  long double result1;
  ios_base::iostate err01 = ios_base::goodbit;
  mon_get.get(is_it01, end, true, iss, err01, result1);
  VERIFY( err01 == ios_base::eofbit );
}

int main()
{
  test01();
  return 0;
}
