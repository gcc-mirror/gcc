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
  bool test __attribute__((unused)) = true;
  typedef time_base::dateorder dateorder;
  typedef istreambuf_iterator<wchar_t> iterator_type;

  // basic construction and sanity checks.
  locale loc_c = locale::classic();
  locale loc_hk = __gnu_test::try_named_locale("en_HK");
  locale loc_fr = __gnu_test::try_named_locale("fr_FR@euro");
  locale loc_de = __gnu_test::try_named_locale("de_DE");
  VERIFY( loc_hk != loc_c );
  VERIFY( loc_hk != loc_fr );
  VERIFY( loc_hk != loc_de );
  VERIFY( loc_de != loc_fr );

  const wstring empty;
  const tm time_bday = { 0, 0, 12, 4, 3, 71, 0, 93, 0 };

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;
  wistringstream iss;
  const time_get<wchar_t>& tim_get = use_facet<time_get<wchar_t> >(iss.getloc()); 
  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // inspection of named locales, en_HK
  iss.imbue(loc_hk);
  iss.str(L"12:00:00 PST"); 
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
