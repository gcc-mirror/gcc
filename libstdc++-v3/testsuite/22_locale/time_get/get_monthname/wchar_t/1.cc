// 2001-09-21 Benjamin Kosnik  <bkoz@redhat.com>

// Copyright (C) 2001, 2002, 2003 Free Software Foundation
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

void test01()
{
  using namespace std;
  typedef time_base::dateorder dateorder;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

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
  const __timepunct<wchar_t>& time_c = use_facet<__timepunct<wchar_t> >(loc_c); 
  const __timepunct<wchar_t>& time_de = use_facet<__timepunct<wchar_t> >(loc_de); 
  const __timepunct<wchar_t>& time_hk = use_facet<__timepunct<wchar_t> >(loc_hk); 
  const __timepunct<wchar_t>& time_fr = use_facet<__timepunct<wchar_t> >(loc_fr); 

  // create "C" time objects
  const tm time_bday = { 0, 0, 12, 4, 3, 71 };

  // iter_type 
  // get_monthname(iter_type, iter_type, ios_base&, 
  //               ios_base::iostate&, tm*) const

  // sanity checks for "C" locale
  iterator_type end;
  wistringstream iss;
  iss.imbue(loc_c);
  const time_get<wchar_t>& tim_get = use_facet<time_get<wchar_t> >(iss.getloc()); 

  iss.str(L"April");
  iterator_type is_it01(iss);
  tm time01;
  errorstate = good;
  tim_get.get_monthname(is_it01, end, iss, errorstate, &time01);
  VERIFY( time01.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str(L"Apr");
  iterator_type is_it02(iss);
  tm time02;
  errorstate = good;
  tim_get.get_monthname(is_it02, end, iss, errorstate, &time02);
  VERIFY( time02.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == ios_base::eofbit );

  iss.str(L"Apr ");
  iterator_type is_it03(iss);
  tm time03;
  errorstate = good;
  tim_get.get_monthname(is_it03, end, iss, errorstate, &time03);
  VERIFY( time03.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == good );
  VERIFY( *is_it03 == ' ');

  iss.str(L"Aar");
  iterator_type is_it04(iss);
  tm time04;
  time04.tm_mon = 5;
  errorstate = good;
  tim_get.get_monthname(is_it04, end, iss, errorstate, &time04);
  VERIFY( time04.tm_mon == 5 );
  VERIFY( *is_it04 == 'a');
  VERIFY( errorstate == ios_base::failbit );

  iss.str(L"December ");
  iterator_type is_it05(iss);
  tm time05;
  errorstate = good;
  tim_get.get_monthname(is_it05, end, iss, errorstate, &time05);
  VERIFY( time05.tm_mon == 11 );
  VERIFY( errorstate == good );
  VERIFY( *is_it05 == ' ');

  iss.str(L"Decelember "); 
  iterator_type is_it06(iss);
  tm time06;
  time06.tm_mon = 4;
  errorstate = good;
  tim_get.get_monthname(is_it06, end, iss, errorstate, &time06);
  VERIFY( time06.tm_mon == 4 );
  VERIFY( errorstate == ios_base::failbit );
  VERIFY( *is_it05 == 'l');
}

int main()
{
  test01();
  return 0;
}
