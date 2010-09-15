// { dg-require-namedlocale "en_HK" }
// { dg-require-namedlocale "de_DE" }

// 2001-09-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2009
// Free Software Foundation
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

// 22.2.5.1.1 time_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test02()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  typedef istreambuf_iterator<wchar_t> iterator_type;

  // basic construction and sanity checks.
  locale loc_c = locale::classic();
  locale loc_hk = locale("en_HK");
  locale loc_de = locale("de_DE");
  VERIFY( loc_hk != loc_c );
  VERIFY( loc_hk != loc_de );

  const wstring empty;

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  wistringstream iss;
  const time_get<wchar_t>& tim_get = use_facet<time_get<wchar_t> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // create "C" time objects
  const tm time_bday = __gnu_test::test_tm(0, 0, 12, 4, 3, 71, 0, 93, 0);

  // iter_type 
  // get_date(iter_type, iter_type, ios_base&, ios_base::iostate&, tm*) const

  // sanity checks for "C" locale
  iss.imbue(loc_c);
  iss.str(L"04/04/71");
  iterator_type is_it01(iss);
  errorstate = good;

  // inspection of named locales, de_DE
  iss.imbue(loc_de);
  iss.str(L"04.04.1971");
  iterator_type is_it10(iss);
  tm time10;
  errorstate = good;
  tim_get.get_date(is_it10, end, iss, errorstate, &time10);
  VERIFY( time10.tm_mon == time_bday.tm_mon );
  VERIFY( time10.tm_mday == time_bday.tm_mday );
  VERIFY( time10.tm_year == time_bday.tm_year );
  VERIFY( errorstate == ios_base::eofbit );

  // inspection of named locales, en_HK
  iss.imbue(loc_hk);
  iss.str(L"Sunday, April 04, 1971"); 
  iterator_type is_it20(iss);
  tm time20;
  errorstate = good;
  tim_get.get_date(is_it20, end, iss, errorstate, &time20);
  VERIFY( time20.tm_mon == time_bday.tm_mon );
  VERIFY( time20.tm_mday == time_bday.tm_mday );
  VERIFY( time20.tm_year == time_bday.tm_year );
  VERIFY( errorstate == ios_base::eofbit );
}

int main()
{
  test02();
  return 0;
}
