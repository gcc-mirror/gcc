// 2001-09-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003, 2004 Free Software Foundation
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

// 22.2.5.1.1 time_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

void test01()
{
  using namespace std;
  bool test __attribute__((unused)) = true;

  typedef istreambuf_iterator<char> iterator_type;

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // basic construction
  locale loc_c = locale::classic();

  // create "C" time objects
  const tm time_bday = { 0, 0, 12, 4, 3, 71, 0, 93, 0 };

  // iter_type 
  // get_monthname(iter_type, iter_type, ios_base&, 
  //               ios_base::iostate&, tm*) const

  // sanity checks for "C" locale
  iterator_type end;
  istringstream iss;
  iss.imbue(loc_c);
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc()); 

  iss.str("April");
  iterator_type is_it01(iss);
  tm time01;
  errorstate = good;
  tim_get.get_monthname(is_it01, end, iss, errorstate, &time01);
  VERIFY( time01.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str("Apr");
  iterator_type is_it02(iss);
  tm time02;
  errorstate = good;
  tim_get.get_monthname(is_it02, end, iss, errorstate, &time02);
  VERIFY( time02.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str("Apr ");
  iterator_type is_it03(iss);
  tm time03;
  errorstate = good;
  iterator_type ret03 = tim_get.get_monthname(is_it03, end, iss, errorstate,
					     &time03);
  VERIFY( time03.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == good );
  VERIFY( *ret03 == ' ' );

  iss.str("Aar");
  iterator_type is_it04(iss);
  tm time04;
  time04.tm_mon = 5;
  errorstate = good;
  iterator_type ret04 = tim_get.get_monthname(is_it04, end, iss, errorstate,
					      &time04);
  VERIFY( time04.tm_mon == 5 );
  VERIFY( *ret04 == 'a' );
  VERIFY( errorstate == ios_base::failbit );

  iss.str("December ");
  iterator_type is_it05(iss);
  tm time05;
  errorstate = good;
  iterator_type ret05 = tim_get.get_monthname(is_it05, end, iss, errorstate,
					      &time05);
  VERIFY( time05.tm_mon == 11 );
  VERIFY( errorstate == good );
  VERIFY( *ret05 == ' ' );

  iss.str("Decelember "); 
  iterator_type is_it06(iss);
  tm time06;
  time06.tm_mon = 4;
  errorstate = good;
  iterator_type ret06 = tim_get.get_monthname(is_it06, end, iss, errorstate,
					      &time06);
  VERIFY( time06.tm_mon == 4 );
  VERIFY( errorstate == ios_base::failbit );
  VERIFY( *ret06 == 'l' );
}

int main()
{
  test01();
  return 0;
}
