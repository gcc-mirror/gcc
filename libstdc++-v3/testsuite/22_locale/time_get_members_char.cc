// 2001-09-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001 Free Software Foundation
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

// 22.2.5.1.1 time_get members

#include <locale>
#include <sstream>
#include <testsuite_hooks.h>

// XXX This test is not working for non-glibc locale models.
// { dg-do run { xfail *-*-* } }

void test01()
{
  using namespace std;
  typedef time_base::dateorder dateorder;
  typedef istreambuf_iterator<char> iterator_type;

  bool test = true;

  // basic construction and sanity checks.
  locale loc_c = locale::classic();
  locale loc_hk("en_HK");
  locale loc_fr("fr_FR@euro");
  locale loc_de("de_DE");
  VERIFY( loc_hk != loc_c );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  // cache the __timepunct facets, for quicker gdb inspection
  const __timepunct<char>& time_c = use_facet<__timepunct<char> >(loc_c); 
  const __timepunct<char>& time_de = use_facet<__timepunct<char> >(loc_de); 
  const __timepunct<char>& time_hk = use_facet<__timepunct<char> >(loc_hk); 
  const __timepunct<char>& time_fr = use_facet<__timepunct<char> >(loc_fr); 

  const string empty;

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  istringstream iss;
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // create "C" time objects
  const tm time_bday = { 0, 0, 12, 4, 3, 71 };
  const char* all = "%a %A %b %B %c %d %H %I %j %m %M %p %s %U "
                    "%w %W %x %X %y %Y %Z %%";
  const char* date = "%A, the second of %B";
  const char* date_ex = "%Ex";

  // 1
  // dateorder date_order() const
  iss.imbue(loc_c);
  dateorder do1 = tim_get.date_order();
  //  VERIFY( do1 == time_base::mdy );
  VERIFY( do1 == time_base::no_order );

  // 2
  // iter_type 
  // get_time(iter_type, iter_type, ios_base&, ios_base::iostate&, tm*) const

  // sanity checks for "C" locale
  iss.str("12:00:00");
  iterator_type is_it01(iss);
  tm time01;
  errorstate = good;
  tim_get.get_time(is_it01, end, iss, errorstate, &time01);
  VERIFY( time01.tm_sec == time_bday.tm_sec );
  VERIFY( time01.tm_min == time_bday.tm_min );
  VERIFY( time01.tm_hour == time_bday.tm_hour );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str("12:00:00 ");
  iterator_type is_it02(iss);
  tm time02;
  errorstate = good;
  tim_get.get_time(is_it02, end, iss, errorstate, &time02);
  VERIFY( time01.tm_sec == time_bday.tm_sec );
  VERIFY( time01.tm_min == time_bday.tm_min );
  VERIFY( time01.tm_hour == time_bday.tm_hour );
  VERIFY( errorstate == good );

  iss.str("12:61:00 ");
  iterator_type is_it03(iss);
  tm time03;
  errorstate = good;
  tim_get.get_time(is_it03, end, iss, errorstate, &time03);
  VERIFY( time01.tm_hour == time_bday.tm_hour );
  VERIFY( errorstate == ios_base::failbit );

  iss.str("12:a:00 ");
  iterator_type is_it04(iss);
  tm time04;
  errorstate = good;
  tim_get.get_time(is_it04, end, iss, errorstate, &time04);
  VERIFY( time01.tm_hour == time_bday.tm_hour );
  VERIFY( *is_it04 == 'a');
  VERIFY( errorstate == ios_base::failbit );

  // inspection of named locales, de_DE
  iss.imbue(loc_de);
  iss.str("12:00:00");
  iterator_type is_it10(iss);
  tm time10;
  errorstate = good;
  tim_get.get_time(is_it10, end, iss, errorstate, &time10);
  VERIFY( time10.tm_sec == time_bday.tm_sec );
  VERIFY( time10.tm_min == time_bday.tm_min );
  VERIFY( time10.tm_hour == time_bday.tm_hour );
  VERIFY( errorstate == ios_base::eofbit );

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
  VERIFY( time10.tm_sec == time_bday.tm_sec );
  VERIFY( time10.tm_min == time_bday.tm_min );
  VERIFY( time10.tm_hour == time_bday.tm_hour );
  VERIFY( errorstate == ios_base::eofbit );
}

void test02()
{
  using namespace std;
  typedef time_base::dateorder dateorder;
  typedef istreambuf_iterator<char> iterator_type;

  bool test = true;

  // basic construction and sanity checks.
  locale loc_c = locale::classic();
  locale loc_hk("en_HK");
  locale loc_fr("fr_FR@euro");
  locale loc_de("de_DE");
  VERIFY( loc_hk != loc_c );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  // cache the __timepunct facets, for quicker gdb inspection
  const __timepunct<char>& time_c = use_facet<__timepunct<char> >(loc_c); 
  const __timepunct<char>& time_de = use_facet<__timepunct<char> >(loc_de); 
  const __timepunct<char>& time_hk = use_facet<__timepunct<char> >(loc_hk); 
  const __timepunct<char>& time_fr = use_facet<__timepunct<char> >(loc_fr); 

  const string empty;

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  istringstream iss;
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // create "C" time objects
  const tm time_bday = { 0, 0, 12, 4, 3, 71 };
  const char* all = "%a %A %b %B %c %d %H %I %j %m %M %p %s %U "
                    "%w %W %x %X %y %Y %Z %%";
  const char* date = "%A, the second of %B";
  const char* date_ex = "%Ex";

  // iter_type 
  // get_weekday(iter_type, iter_type, ios_base&, 
  //             ios_base::iostate&, tm*) const

  // sanity checks for "C" locale
  iss.str("Sunday");
  iterator_type is_it01(iss);
  tm time01;
  errorstate = good;
  tim_get.get_weekday(is_it01, end, iss, errorstate, &time01);
  VERIFY( time01.tm_wday == time_bday.tm_wday );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str("Sun");
  iterator_type is_it02(iss);
  tm time02;
  errorstate = good;
  tim_get.get_weekday(is_it02, end, iss, errorstate, &time02);
  VERIFY( time02.tm_wday == time_bday.tm_wday );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str("Sun ");
  iterator_type is_it03(iss);
  tm time03;
  errorstate = good;
  tim_get.get_weekday(is_it03, end, iss, errorstate, &time03);
  VERIFY( time03.tm_wday == time_bday.tm_wday );
  VERIFY( errorstate == good );
  VERIFY( *is_it03 == ' ');

  iss.str("San");
  iterator_type is_it04(iss);
  tm time04;
  time04.tm_wday = 4;
  errorstate = good;
  tim_get.get_weekday(is_it04, end, iss, errorstate, &time04);
  VERIFY( time04.tm_wday == 4 );
  VERIFY( *is_it04 == 'n');
  VERIFY( errorstate == ios_base::failbit );

  iss.str("Tuesday ");
  iterator_type is_it05(iss);
  tm time05;
  errorstate = good;
  tim_get.get_weekday(is_it05, end, iss, errorstate, &time05);
  VERIFY( time05.tm_wday == 2 );
  VERIFY( errorstate == good );
  VERIFY( *is_it05 == ' ');

  iss.str("Tuesducky "); // Kind of like Fryday, without the swirls.
  iterator_type is_it06(iss);
  tm time06;
  time06.tm_wday = 4;
  errorstate = good;
  tim_get.get_weekday(is_it06, end, iss, errorstate, &time06);
  VERIFY( time06.tm_wday == 4 );
  VERIFY( errorstate == ios_base::failbit );
  VERIFY( *is_it05 == 'u');

  // inspection of named locales, de_DE
  iss.imbue(loc_de);
  iss.str("Sonntag");
  iterator_type is_it10(iss);
  tm time10;
  errorstate = good;
  tim_get.get_weekday(is_it10, end, iss, errorstate, &time10);
  VERIFY( time10.tm_wday == time_bday.tm_wday );
  VERIFY( errorstate == ios_base::eofbit );

  // inspection of named locales, en_HK
  iss.imbue(loc_hk);
  iss.str("Sunday"); 
  iterator_type is_it20(iss);
  tm time20;
  errorstate = good;
  tim_get.get_weekday(is_it20, end, iss, errorstate, &time20);
  VERIFY( time20.tm_wday == time_bday.tm_wday );
  VERIFY( errorstate == ios_base::eofbit );
}

void test03()
{
  using namespace std;
  typedef time_base::dateorder dateorder;
  typedef istreambuf_iterator<char> iterator_type;

  bool test = true;

  // basic construction and sanity checks.
  locale loc_c = locale::classic();
  locale loc_hk("en_HK");
  locale loc_fr("fr_FR@euro");
  locale loc_de("de_DE");
  VERIFY( loc_hk != loc_c );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  // cache the __timepunct facets, for quicker gdb inspection
  const __timepunct<char>& time_c = use_facet<__timepunct<char> >(loc_c); 
  const __timepunct<char>& time_de = use_facet<__timepunct<char> >(loc_de); 
  const __timepunct<char>& time_hk = use_facet<__timepunct<char> >(loc_hk); 
  const __timepunct<char>& time_fr = use_facet<__timepunct<char> >(loc_fr); 

  const string empty;

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  istringstream iss;
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // create "C" time objects
  const tm time_bday = { 0, 0, 12, 4, 3, 71 };
  const char* all = "%a %A %b %B %c %d %H %I %j %m %M %p %s %U "
                    "%w %W %x %X %y %Y %Z %%";
  const char* date = "%A, the second of %B";
  const char* date_ex = "%Ex";

  // iter_type 
  // get_monthname(iter_type, iter_type, ios_base&, 
  //               ios_base::iostate&, tm*) const

  // sanity checks for "C" locale
  iss.str("April");
  iterator_type is_it01(iss);
  tm time01;
  errorstate = good;
  tim_get.get_monthname(is_it01, end, iss, errorstate, &time01);
  VERIFY( time01.tm_wday == time_bday.tm_wday );
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
  tim_get.get_monthname(is_it03, end, iss, errorstate, &time03);
  VERIFY( time03.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == good );
  VERIFY( *is_it03 == ' ');

  iss.str("Aar");
  iterator_type is_it04(iss);
  tm time04;
  time04.tm_mon = 5;
  errorstate = good;
  tim_get.get_monthname(is_it04, end, iss, errorstate, &time04);
  VERIFY( time04.tm_mon == 5 );
  VERIFY( *is_it04 == 'a');
  VERIFY( errorstate == ios_base::failbit );

  iss.str("December ");
  iterator_type is_it05(iss);
  tm time05;
  errorstate = good;
  tim_get.get_monthname(is_it05, end, iss, errorstate, &time05);
  VERIFY( time05.tm_mon == 11 );
  VERIFY( errorstate == good );
  VERIFY( *is_it05 == ' ');

  iss.str("Decelember "); 
  iterator_type is_it06(iss);
  tm time06;
  time06.tm_mon = 4;
  errorstate = good;
  tim_get.get_monthname(is_it06, end, iss, errorstate, &time06);
  VERIFY( time06.tm_mon == 4 );
  VERIFY( errorstate == ios_base::failbit );
  VERIFY( *is_it05 == 'l');

  // inspection of named locales, de_DE
  iss.imbue(loc_de);
  iss.str("April");
  iterator_type is_it10(iss);
  tm time10;
  errorstate = good;
  tim_get.get_monthname(is_it10, end, iss, errorstate, &time10);
  VERIFY( time10.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == ios_base::eofbit );

  // inspection of named locales, en_HK
  iss.imbue(loc_hk);
  iss.str("April"); 
  iterator_type is_it20(iss);
  tm time20;
  errorstate = good;
  tim_get.get_monthname(is_it20, end, iss, errorstate, &time20);
  VERIFY( time20.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == ios_base::eofbit );
}

void test04()
{
  using namespace std;
  typedef time_base::dateorder dateorder;
  typedef istreambuf_iterator<char> iterator_type;

  bool test = true;

  // basic construction and sanity checks.
  locale loc_c = locale::classic();
  locale loc_hk("en_HK");
  locale loc_fr("fr_FR@euro");
  locale loc_de("de_DE");
  VERIFY( loc_hk != loc_c );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  // cache the __timepunct facets, for quicker gdb inspection
  const __timepunct<char>& time_c = use_facet<__timepunct<char> >(loc_c); 
  const __timepunct<char>& time_de = use_facet<__timepunct<char> >(loc_de); 
  const __timepunct<char>& time_hk = use_facet<__timepunct<char> >(loc_hk); 
  const __timepunct<char>& time_fr = use_facet<__timepunct<char> >(loc_fr); 

  const string empty;

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  istringstream iss;
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // create "C" time objects
  const tm time_bday = { 0, 0, 12, 4, 3, 71 };
  const char* all = "%a %A %b %B %c %d %H %I %j %m %M %p %s %U "
                    "%w %W %x %X %y %Y %Z %%";
  const char* date = "%A, the second of %B";
  const char* date_ex = "%Ex";

  // iter_type 
  // get_year(iter_type, iter_type, ios_base&, ios_base::iostate&, tm*) const

  // sanity checks for "C" locale
  iss.str("1971");
  iterator_type is_it01(iss);
  tm time01;
  errorstate = good;
  tim_get.get_year(is_it01, end, iss, errorstate, &time01);
  VERIFY( time01.tm_year == time_bday.tm_year );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str("1971 ");
  iterator_type is_it02(iss);
  tm time02;
  errorstate = good;
  tim_get.get_year(is_it02, end, iss, errorstate, &time02);
  VERIFY( time02.tm_year == time_bday.tm_year );
  VERIFY( errorstate == good );
  VERIFY( *is_it02 == ' ');

  iss.str("197d1 ");
  iterator_type is_it03(iss);
  tm time03;
  time03.tm_year = 3;
  errorstate = good;
  tim_get.get_year(is_it03, end, iss, errorstate, &time03);
  VERIFY( time03.tm_year == 3 );
  VERIFY( errorstate == ios_base::failbit );
  VERIFY( *is_it03 == 'd');

  iss.str("71d71");
  iterator_type is_it04(iss);
  tm time04;
  errorstate = good;
  tim_get.get_year(is_it04, end, iss, errorstate, &time04);
  VERIFY( time04.tm_year == time_bday.tm_year );
  VERIFY( errorstate == good );
  VERIFY( *is_it03 == 'd');

  iss.str("71");
  iterator_type is_it05(iss);
  tm time05;
  errorstate = good;
  tim_get.get_year(is_it05, end, iss, errorstate, &time05);
  VERIFY( time05.tm_year == time_bday.tm_year );
  VERIFY( errorstate == ios_base::eofbit );
}

int main()
{
  test01();
  test02();
  test03();
  test04();
  return 0;
}
