// { dg-require-namedlocale "" }

// Copyright (C) 2003, 2005 Free Software Foundation
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

void test01()
{
  using namespace std;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  bool test __attribute__((unused)) = true;

  locale loc_us = locale("en_US");

  iterator_type end;
  wistringstream iss;
  iss.imbue(loc_us);

  const money_get<wchar_t>& mon_get = use_facet<money_get<wchar_t> >(iss.getloc());

  iss.str(L"$.00 ");
  iterator_type is_it(iss);
  wstring extracted_amount;
  ios_base::iostate err = ios_base::goodbit;
  mon_get.get(is_it, end, false, iss, err, extracted_amount);
  VERIFY( extracted_amount == L"0" );
  VERIFY( err == ios_base::goodbit );
}

int main()
{
  test01();
  return 0;
}
