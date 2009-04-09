// { dg-require-namedlocale "" }

// 2003-10-24  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2003, 2005, 2009 Free Software Foundation
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
  typedef istreambuf_iterator<char> iterator_type;

  bool test __attribute__((unused)) = true;
  
  // basic construction
  locale loc_de = locale("de_DE@euro");

  iterator_type end;
  istringstream iss;
  iss.imbue(loc_de);

  // cache the money_get facet
  const money_get<char>& mon_get = use_facet<money_get<char> >(iss.getloc()); 

  // A _very_ big amount.
  string str = "1";
  for (int i = 0; i < 2 * numeric_limits<long double>::digits10; ++i)
    str += ".000";
  str += ",00 ";

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
