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

void test02()
{
  using namespace std;
  typedef time_base::dateorder dateorder;
  typedef istreambuf_iterator<char> iterator_type;

  bool test __attribute__((unused)) = true;

  // basic construction and sanity checks.
  locale loc_c = locale::classic();
  locale loc_hk = __gnu_test::try_named_locale("en_HK");
  locale loc_fr = __gnu_test::try_named_locale("fr_FR@euro");
  locale loc_de = __gnu_test::try_named_locale("de_DE");
  VERIFY( loc_hk != loc_c );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  const tm time_bday = { 0, 0, 12, 4, 3, 71, 0, 93, 0 };

  const string empty;

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  istringstream iss;
  iss.imbue(loc_de);
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc()); 

  // inspection of named locales, de_DE
  iss.str("April");
  iterator_type is_it10(iss);
  tm time10;
  errorstate = good;
  tim_get.get_monthname(is_it10, end, iss, errorstate, &time10);
  VERIFY( time10.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == ios_base::eofbit );

  // inspection of named locales, en_HK
  iss.imbue(loc_hk);
  const time_get<char>& tim_get2 = use_facet<time_get<char> >(iss.getloc()); 
  iss.str("April"); 
  iterator_type is_it20(iss);
  tm time20;
  errorstate = good;
  tim_get2.get_monthname(is_it20, end, iss, errorstate, &time20);
  VERIFY( time20.tm_mon == time_bday.tm_mon );
  VERIFY( errorstate == ios_base::eofbit );
}

int main()
{
  test02();
  return 0;
}
