// { dg-require-namedlocale "ru_RU.ISO8859-5" }

// Copyright (C) 2010-2020 Free Software Foundation, Inc.
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
#include <cstring>
#include <testsuite_hooks.h>

// libstdc++/38081
void test01()
{
  using namespace std;

  typedef istreambuf_iterator<char> iterator_type;

  // basic construction
  locale loc(ISO_8859(5,ru_RU));

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  istringstream iss;
  iss.imbue(loc);
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // iter_type 
  // get_weekday(iter_type, iter_type, ios_base&, 
  //             ios_base::iostate&, tm*) const

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 14)
# if __GLIBC__ > 2 || __GLIBC_MINOR__ >= 17
  iss.str("\xbf\xdd");
# else
  iss.str("\xbf\xdd\x2e");
# endif
#else
  iss.str("\xbf\xdd\xd4");
#endif
  iterator_type is_it01(iss);
  tm time01;
  memset(&time01, -1, sizeof(tm));
  errorstate = good;
  tim_get.get_weekday(is_it01, end, iss, errorstate, &time01);
  VERIFY( time01.tm_wday == 1 );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str("\xbf\xde\xdd\xd5\xd4\xd5\xdb\xec\xdd\xd8\xda");
  iterator_type is_it02(iss);
  tm time02;
  memset(&time02, -1, sizeof(tm));
  errorstate = good;
  tim_get.get_weekday(is_it02, end, iss, errorstate, &time02);
  VERIFY( time02.tm_wday == 1 );
  VERIFY( errorstate == ios_base::eofbit );

#if __GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 14)
# if __GLIBC__ > 2 || __GLIBC_MINOR__ >= 17
  iss.str("\xbf\xdd\xd5\xd4\xd5\xdb\xec\xdd\xd8\xda");
# else
  iss.str("\xbf\xdd\x2e\xd5\xd4\xd5\xdb\xec\xdd\xd8\xda");
# endif
#else
  iss.str("\xbf\xdd\xd4\xd5\xd4\xd5\xdb\xec\xdd\xd8\xda");
#endif
  iterator_type is_it03(iss);
  tm time03;
  memset(&time03, -1, sizeof(tm));
  errorstate = good;
  iterator_type ret = tim_get.get_weekday(is_it03, end, iss,
					  errorstate, &time03);
  VERIFY( time03.tm_wday == 1 );
  VERIFY( errorstate == ios_base::goodbit );
  VERIFY( *ret == '\xd5' );
}

int main()
{
  test01();
  return 0;
}
