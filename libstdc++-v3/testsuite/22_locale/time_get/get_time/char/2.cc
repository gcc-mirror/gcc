// { dg-require-namedlocale "en_HK.ISO8859-1" }

// 2001-09-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001-2020 Free Software Foundation, Inc.
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

  typedef istreambuf_iterator<char> iterator_type;

  // basic construction and sanity check
  locale loc_c = locale::classic();
  locale loc_hk = locale(ISO_8859(1,en_HK));
  VERIFY( loc_hk != loc_c );

  const string empty;
  const tm time_bday = __gnu_test::test_tm(0, 0, 12, 4, 3, 71, 0, 93, 0);

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;
  istringstream iss;
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc()); 
  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // inspection of named locales, en_HK
  iss.imbue(loc_hk);
  iss.str("12:00:00 PST"); 
  // Hong Kong in California! Well, they have Paris in Vegas... this
  // is all a little disney-esque anyway. Besides, you can get decent
  // Dim Sum in San Francisco.
  iterator_type is_it20(iss);
  tm time20;
  errorstate = good;
  tim_get.get_time(is_it20, end, iss, errorstate, &time20);
  VERIFY( time20.tm_sec == time_bday.tm_sec );
  VERIFY( time20.tm_min == time_bday.tm_min );
  VERIFY( time20.tm_hour == time_bday.tm_hour );
  VERIFY( errorstate == ios_base::eofbit );
}

int main()
{
  test02();
  return 0;
}
