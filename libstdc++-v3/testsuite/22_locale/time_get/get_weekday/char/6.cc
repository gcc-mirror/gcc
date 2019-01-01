// 2010-01-05  Paolo Carlini  <paolo.carlini@oracle.com>

// Copyright (C) 2010-2019 Free Software Foundation, Inc.
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

void test01()
{
  using namespace std;

  typedef istreambuf_iterator<char> iterator_type;

  // basic construction
  locale loc_c = locale::classic();

  // create an ostream-derived object, cache the time_get facet
  iterator_type end;

  istringstream iss;
  iss.imbue(loc_c);
  const time_get<char>& tim_get = use_facet<time_get<char> >(iss.getloc()); 

  const ios_base::iostate good = ios_base::goodbit;
  ios_base::iostate errorstate = good;

  // iter_type 
  // get_weekday(iter_type, iter_type, ios_base&, 
  //             ios_base::iostate&, tm*) const

  // sanity checks for "C" locale

  const char* awdays[7] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };

  for (int i = 0; i < 7; ++i)
    {
      iss.str(awdays[i]);
      iterator_type is_it01(iss);
      tm time01;
      memset(&time01, -1, sizeof(tm));
      errorstate = good;
      tim_get.get_weekday(is_it01, end, iss, errorstate, &time01);
      VERIFY( time01.tm_wday == i );
      VERIFY( errorstate == ios_base::eofbit );
    }

  const char* wdays[7] = { "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
			   "Friday", "Saturday" };

  for (int i = 0; i < 7; ++i)
    {
      iss.str(wdays[i]);
      iterator_type is_it01(iss);
      tm time01;
      memset(&time01, -1, sizeof(tm));
      errorstate = good;
      tim_get.get_weekday(is_it01, end, iss, errorstate, &time01);
      VERIFY( time01.tm_wday == i );
      VERIFY( errorstate == ios_base::eofbit );
    }
}

int main()
{
  test01();
  return 0;
}
