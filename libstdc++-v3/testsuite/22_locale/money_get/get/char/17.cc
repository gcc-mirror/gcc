// 2004-03-08  Paolo Carlini  <pcarlini@suse.de>

// Copyright (C) 2004 Free Software Foundation
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

// 22.2.6.1.1 money_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// 22.2.6.3, p2: "The value _space_ indicates that at least one space
//                is required at that position."
void test01()
{
  using namespace std;
  typedef istreambuf_iterator<char> iterator_type;

  bool test __attribute__((unused)) = true;

  // basic construction
  locale loc_c = locale::classic();
  locale loc_de = __gnu_test::try_named_locale("de_DE@euro");
  VERIFY( loc_c != loc_de );

  iterator_type end, end02;
  istringstream iss;
  iss.imbue(loc_de);
  // cache the money_get facet
  const money_get<char>& mon_get =
    use_facet<money_get<char> >(iss.getloc()); 

  iss.str("7.200.000.000,00");
  iterator_type is_it01(iss);
  string result1;
  ios_base::iostate err01 = ios_base::goodbit;
  mon_get.get(is_it01, end, true, iss, err01, result1);
  VERIFY( err01 == (ios_base::failbit | ios_base::eofbit) );

  // now try with showbase, to get currency symbol in format
  iss.setf(ios_base::showbase);

  iss.str("7.200.000.000,00EUR ");
  iterator_type is_it02(iss);
  string result2;
  ios_base::iostate err02 = ios_base::goodbit;
  end02 = mon_get.get(is_it02, end, true, iss, err02, result2);
  VERIFY( err02 == ios_base::failbit );
  VERIFY( *end02 == 'E' );
}

int main()
{
  test01();
  return 0;
}
